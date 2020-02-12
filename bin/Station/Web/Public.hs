module Station.Web.Public (handle) where

import Prelude ()
import Data.Bool (otherwise)
import Data.Eq ((==), (/=))
import Data.Maybe (Maybe (Just))
import Data.Tuple (fst)
import Data.List (lookup)
import Data.Functor ((<$>))
import Control.Monad ((=<<))
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Binary.Builder as BS.Builder
import Data.Time.Clock (secondsToDiffTime)
import qualified Network.HTTP.Types
import qualified Network.HTTP.Types.Header
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse
import qualified Network.Wai.Middleware.HttpAuth as HttpAuth
import qualified Web.Cookie as Cookie

import qualified Station.Constant as Constant
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.Database.Session as DB.Session
import qualified Station.HTTP as HTTP

handle_login :: DB.Type -> Wai.Application
handle_login database request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (lookup "username" parameters, lookup "password" parameters) of
				(Just username', Just password') ->
					(\case
						[] -> HTTP.respond_303 Constant.login_fail request respond
						user : _ ->
							(\case
								session : _ ->
									do
										let
											cookie =
												Cookie.defaultSetCookie{
													Cookie.setCookieName = Constant.session,
													Cookie.setCookieValue = BS.C8.pack session,
													Cookie.setCookiePath = Just "/",
													Cookie.setCookieMaxAge = Just (secondsToDiffTime Constant.session_age)}
											setCookie = BS.L.toStrict (BS.Builder.toLazyByteString (Cookie.renderSetCookie cookie))
											headers = [( Network.HTTP.Types.Header.hSetCookie, setCookie)]
										HTTP.respond_303_headers headers Constant.private_home request respond
								_ -> HTTP.respond_500 "Cannot create session" request respond)
								=<< DB.Session.add user database)
						=<< DB.User.check_password (BS.U8.toString username') password' database
				_ -> HTTP.respond_303 Constant.login_fail request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_logout :: DB.Type -> Wai.Application
handle_logout _database request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		case HttpAuth.extractBasicAuth =<< lookup Network.HTTP.Types.hAuthorization (Wai.requestHeaders request) of
			Just auth
				| fst auth /= "" ->
					HTTP.respond_401 "Empty user name to logout" request respond
			_ -> HTTP.respond_303 Constant.public_home request respond
	| otherwise =
		HTTP.respond_405 request respond

handle :: DB.Type -> Wai.Middleware
handle database next request respond =
	case Wai.pathInfo request of
		["public", "login"] -> handle_login database request respond
		["public", "logout"] -> handle_logout database request respond
		_ -> next request respond
