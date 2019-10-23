module Main (main) where

import Prelude ()
import Data.Bool (Bool (True, False))
import Data.Maybe (fromMaybe)
import Data.List ((++), any)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Control.Monad (return, (>>=), (=<<))
import Text.Show (show)
import Text.Read (readMaybe)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (FilePath)
import System.IO (IO, stdout, putStrLn, BufferMode (LineBuffering), hSetBuffering)
import System.Environment (getArgs, lookupEnv)
import qualified Data.ByteString.Char8 as BS.C8
import qualified Crypto.Scrypt
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.HttpAuth as HttpAuth
import qualified Network.Wai.Application.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.PostgreSQL.Simple as DB

import qualified Station.Constant as Constant
import qualified Station.HTTP as HTTP
import qualified Station.Web as Web

{- -------------------------------------------------------------------------------------------------------------------------- -}

default_port :: Warp.Port
default_port = 8000

static_path :: FilePath
static_path = "www"

{- -------------------------------------------------------------------------------------------------------------------------- -}

handle_home :: Wai.Middleware
handle_home next request =
	case Wai.pathInfo request of
		[] -> HTTP.respond_301 Constant.public_home request
		_ -> next request

auth_check :: DB.Connection -> Wai.Middleware
auth_check db =
	let
		protect :: Wai.Request -> IO Bool
		protect request =
			case Wai.pathInfo request of
				"bin" : "logout" : [] -> return False
				"public" : _ : _ -> return False
				"home.xhtml" : [] -> return False
				_ -> return True
		check :: HttpAuth.CheckCreds
		check user password =
			do
				hashed <- DB.query db "SELECT \"PASSWORD\" FROM \"USER\" WHERE \"NAME\"=?" (DB.Only user)
				let
					pass :: Crypto.Scrypt.Pass
					pass = Crypto.Scrypt.Pass password
				return (any (Crypto.Scrypt.verifyPass' pass . Crypto.Scrypt.EncryptedPass . DB.fromOnly) hashed)
		realm :: HttpAuth.AuthSettings
		realm = Constant.auth_realm{HttpAuth.authIsProtected = protect}
		in HttpAuth.basicAuth check realm

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
						HTTP.log log $
						handle_home $
						auth_check db $
						Web.handle db $
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
