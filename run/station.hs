module Main (main) where

import Prelude ()
import Data.Bool (Bool (True, False))
import Data.Eq ((==), (/=))
import Data.Ord ((>))
import Data.Maybe (Maybe (Just), maybe, fromMaybe)
import Data.Tuple (fst)
import Data.List ((++), any, lookup)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.String (String, fromString)
import Control.Monad (return, (>>=), (=<<))
import Text.Show (show)
import Text.Read (readMaybe)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (FilePath)
import System.IO (IO, stdout, putChar, putStr, putStrLn, BufferMode (LineBuffering), hSetBuffering)
import System.Environment (getArgs, lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.ByteString.Lazy.UTF8 as BS.L.U8
import qualified Text.XML.Light as XML
import qualified Crypto.Scrypt
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.HttpAuth as HttpAuth
import qualified Network.Wai.Application.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Parse as Wai.Parse
import qualified Database.PostgreSQL.Simple as DB

{- -------------------------------------------------------------------------------------------------------------------------- -}

default_port :: Warp.Port
default_port = 8000

static_path :: FilePath
static_path = "www"

home_page :: BS.ByteString
home_page = "/home.xhtml"

auth_realm :: String
auth_realm = "Station"

{- -------------------------------------------------------------------------------------------------------------------------- -}

http_log :: Bool -> Wai.Middleware
http_log False next request respond = next request respond
http_log True  next request respond =
	do
		putStr . formatTime defaultTimeLocale "%FT%T%z" =<< getZonedTime
		putStr " HTTP "
		putStr (show (Wai.remoteHost request))
		putChar ' '
		BS.C8.putStr (Wai.requestMethod request)
		putChar ' '
		putStr
			(maybe
				"()"
				(show . BS.U8.toString . fst)
				(HttpAuth.extractBasicAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders request)))
		putChar ' '
		BS.C8.putStrLn (Wai.rawPathInfo request)
		next request respond

respond_200 :: Wai.Application
respond_200 _ respond =
	respond (Wai.responseLBS HTTP.status200 [("Content-Type", "text/plain; charset=ASCII")] "OK")

respond_404 :: Wai.Application
respond_404 _ respond =
	respond (Wai.responseLBS HTTP.status404 [("Content-Type", "text/plain; charset=ASCII")] "NOT FOUND")

respond_redirect :: HTTP.Status -> BS.L.ByteString -> BS.ByteString -> Wai.Application
respond_redirect status text location _ respond =
	respond (Wai.responseLBS status [("Content-Type", "text/plain; charset=ASCII"), ("Location", location)] text)

respond_301 :: BS.ByteString -> Wai.Application
respond_301 = respond_redirect HTTP.status301 "MOVED PERMANENTLY"

respond_303 :: BS.ByteString -> Wai.Application
respond_303 = respond_redirect HTTP.status303 "SEE OTHER"

respond_XML :: XML.Element -> Wai.Application
respond_XML xml _ respond =
	respond
		(Wai.responseLBS
			HTTP.status200
			[("Content-Type", "text/xml; charset=UTF-8")]
			(BS.L.U8.fromString (XML.showElement xml)))

get_user :: Wai.Request -> Maybe BS.ByteString
get_user request = fst <$> (HttpAuth.extractBasicAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders request))

handle_home :: Wai.Middleware
handle_home next request =
	case Wai.pathInfo request of
		[] -> respond_301 home_page request
		_ -> next request

auth_check :: DB.Connection -> Wai.Middleware
auth_check db =
	let
		protect :: Wai.Request -> IO Bool
		protect request =
			case Wai.pathInfo request of
				"bin" : "logout" : _ -> return False
				"bin" : _ -> return True
				_ -> return False
		check :: HttpAuth.CheckCreds
		check user password =
			do
				password's <- DB.query db "SELECT \"PASSWORD\" FROM \"USER\" WHERE \"NAME\"=?" (DB.Only user)
				let
					pass :: Crypto.Scrypt.Pass
					pass = Crypto.Scrypt.Pass password
				return (any (Crypto.Scrypt.verifyPass' pass . Crypto.Scrypt.EncryptedPass . DB.fromOnly) password's)
		realm :: HttpAuth.AuthSettings
		realm = (fromString auth_realm){HttpAuth.authIsProtected = protect}
		in HttpAuth.basicAuth check realm

command_password :: DB.Connection -> Wai.Application
command_password db request respond =
	do
		let user' = get_user request
		parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
		case (user', lookup "password" parameters) of
			(Just user, Just password)
				| BS.length password > 0 ->
					do
						encrypted <- Crypto.Scrypt.encryptPassIO' (Crypto.Scrypt.Pass password)
						n <-
							DB.execute
								db
								"UPDATE \"USER\" SET \"PASSWORD\"=? WHERE \"NAME\"=?"
								(Crypto.Scrypt.getEncryptedPass encrypted, user)
						case n of
							1 -> respond_200 request respond
							_ -> respond_404 request respond
			_ -> respond_404 request respond

command_login :: Wai.Application
command_login = respond_303 home_page

command_logout :: Wai.Application
command_logout request respond
	|
		maybe
			True
			(("anonymous", "anonymous") ==)
			(HttpAuth.extractBasicAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders request))
		= respond_303 home_page request respond
command_logout _ respond =
	respond
		(Wai.responseLBS
			HTTP.status401
			[
				("Content-Type", "text/plain; charset=ASCII"),
				("WWW-Authenticate", fromString ("Basic realm=\"" ++ auth_realm ++ "\", charset=\"UTF-8\""))]
			"UNAUTHORIZED")

handle_POST :: DB.Connection -> Wai.Middleware
handle_POST _ next request respond
	| Wai.requestMethod request /= HTTP.methodPost =
		next request respond
handle_POST db _ request respond =
	case Wai.pathInfo request of
		["bin", command] ->
			case command of
				"password" -> command_password db request respond
				"login" -> command_login request respond
				"logout" -> command_logout request respond
				_ -> respond_404 request respond
		_ -> respond_404 request respond

main :: IO ()
main =
	do
		hSetBuffering stdout LineBuffering
		port <- fromMaybe default_port <$> (readMaybe =<<) <$> lookupEnv "PORT"
		dburl <- fromMaybe "" <$> lookupEnv "DATABASE_URL"
		log <- fromMaybe False <$> (readMaybe =<<) <$> lookupEnv "LOG"
		putStrLn "Configuration"
		putStrLn ("\tport: " ++ show port)
		putStrLn ("\tdatabase: " ++ show dburl)
		putStrLn ("\tlog: " ++ show log)
		getArgs >>= \case
			[] ->
				do
					db <- DB.connectPostgreSQL (BS.C8.pack dburl)
					let application =
						http_log log $
						handle_home $
						auth_check db $
						handle_POST db $
						Wai.Static.staticApp (Wai.Static.defaultWebAppSettings static_path)
					Warp.run port application
			["adduser", username@(_:_), password@(_:_)] ->
				do
					db <- DB.connectPostgreSQL (BS.C8.pack dburl)
					encrypted <- Crypto.Scrypt.encryptPassIO' (Crypto.Scrypt.Pass (BS.C8.pack password))
					n <-
						DB.execute
							db
							"INSERT INTO \"USER\"(\"NAME\",\"PASSWORD\") VALUES(?,?)"
							(username, Crypto.Scrypt.getEncryptedPass encrypted)
					case n of
						1 ->
							do
								putStrLn "OK"
								exitSuccess
						_ ->
							do
								putStrLn "Fail"
								exitFailure
			_ ->
				do
					putStrLn "Commands:"
					putStrLn "\t(no argument): start server"
					putStrLn "\tadduser {username} {password}: add a new user"

{- -------------------------------------------------------------------------------------------------------------------------- -}
