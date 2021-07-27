module Station.Web (handle) where

import Prelude ()
import Data.Eq ((==))
import Data.Ord ((>))
import Data.Maybe (Maybe (Just), fromJust)
import Data.Tuple (fst, snd)
import Data.List (lookup)
import Data.Functor ((<$>))
import Control.Monad ((>>=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Text
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse

import qualified Station.Constant as Constant
import qualified Station.XML as XML
import qualified Station.Database.User as DB.User
import qualified Station.HTTP as HTTP
import qualified Station.Web.Environment as Environment
import qualified Station.Web.Tool as Web.Tool
import qualified Station.Web.SysOp as Web.SysOp
import qualified Station.Web.Learn as Web.Learn

handle_account :: Environment.Type -> Wai.Application
handle_account Environment.Record{Environment.user = Just user} request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		let
			user_name = DB.User.name user
			xml = XML.element "account" [("name", user_name)] [Web.Tool.user_XML user]
			body = XML.xslt "account.xsl" xml
			in HTTP.respond_XML body request respond
handle_account environment@Environment.Record{Environment.user = Just user} request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case lookup "password" parameters of
				Just password
					| BS.length password > 0 ->
						do
							ok <-
								DB.User.set_password
									(DB.User.name user)
									(BS.U8.toString password)
									(Environment.database environment)
							if ok
								then HTTP.respond_303 Constant.private_home request respond
								else HTTP.respond_400 "Failed to update password" request respond
				_ -> HTTP.respond_404 request respond
handle_account _ request respond =
	HTTP.respond_405 request respond

handle_avatar :: Environment.Type -> Data.Text.Text -> Wai.Application
handle_avatar environment user_name request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		let output name =
			DB.User.get_avatar name (Environment.database environment) >>= \case
				[avatar] ->
					respond
						(Wai.responseLBS Network.HTTP.Types.status200
							[("Content-Type", "image/png")]
							(BS.L.fromStrict avatar))
				_ -> HTTP.respond_404 request respond
			in
			case (environment, user_name) of
				(Environment.Record{Environment.user = Just user}, "") -> output (DB.User.name user)
				(_, _) -> output (Data.Text.unpack user_name)
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case lookup "avatar" parameters of
				Just avatar
					| BS.length avatar > 0 ->
						do
							ok <- DB.User.set_avatar
											(DB.User.name user)
											(snd avatar)
											(Environment.database environment)
							if ok
								then 
									let
										user = (fromJust (Environment.user session))
										user_name = DB.User.name user
										xml = XML.element "account" [("name", user_name)] [Web.Tool.user_XML user]
										body = XML.xslt "account.xsl" xml
										in HTTP.respond_XML body request respond
								else HTTP.respond_400 "Failed to set avatar" request respond

handle_avatar _ _ request respond =
	HTTP.respond_405 request respond

handle :: Environment.Type -> Wai.Middleware
handle environment next request respond =
	case Wai.pathInfo request of
		[] -> HTTP.respond_301 Constant.public_home request respond
		"sysop" : path -> Web.SysOp.handle environment path next request respond
		"account" : [] -> handle_account environment request respond
		"avatar" : user_name : [] -> handle_avatar environment user_name request respond
		"learn" : path -> Web.Learn.handle environment path next request respond
		_ -> next request respond
