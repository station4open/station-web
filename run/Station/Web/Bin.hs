module Station.Web.Bin (handle) where

import Prelude ()
import Data.Bool (otherwise)
import Data.Eq ((==), (/=))
import Data.Ord ((>))
import Data.Maybe (Maybe (Just))
import Data.Tuple (fst)
import Data.List (lookup)
import Data.Functor ((<$>))
import Control.Monad ((=<<))
import qualified Data.ByteString as BS
import qualified Data.Text (Text)
import qualified Crypto.Scrypt
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.HttpAuth as HttpAuth
import qualified Network.Wai.Parse as Wai.Parse
import qualified Database.PostgreSQL.Simple as DB

--	import qualified Station.Database as DB
import qualified Station.Constant as Constant
import qualified Station.HTTP as HTTP

handle_password :: DB.Connection -> Wai.Application
handle_password db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (HTTP.auth_user request, lookup "password" parameters) of
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
								1 -> HTTP.respond_200 request respond
								_ -> HTTP.respond_404 request respond
				_ -> HTTP.respond_404 request respond
	| otherwise =
		HTTP.respond_404 request respond

handle_login :: Wai.Application
handle_login request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		HTTP.respond_303 Constant.private_home request respond
	| otherwise =
		HTTP.respond_404 request respond

handle_logout :: Wai.Application
handle_logout request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		case HttpAuth.extractBasicAuth =<< lookup Network.HTTP.Types.hAuthorization (Wai.requestHeaders request) of
			Just auth
				| auth /= ("anonymous", "anonymous") ->
					HTTP.respond_401 request respond
			_ -> HTTP.respond_303 Constant.public_home request respond
	| otherwise =
		HTTP.respond_404 request respond

handle :: [Data.Text.Text] -> DB.Connection -> Wai.Middleware
handle path db next =
	case path of
		["password"] -> handle_password db
		["login"] -> handle_login
		["logout"] -> handle_logout
		_ -> next
