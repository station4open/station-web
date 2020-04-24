module Station.Web (handle) where

import Prelude ()
import Data.Eq ((==))
import Data.Ord ((>))
import Data.Maybe (Maybe (Just), maybeToList)
import Data.Tuple (fst)
import Data.List ((++), map, lookup)
import Data.Functor ((<$>))
import Text.Show (show)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse

import qualified Station.Constant as Constant
import qualified Station.XML as XML
import qualified Station.Database.User as DB.User
import qualified Station.Database.Subject as DB.Subject
import qualified Station.HTTP as HTTP
import qualified Station.Web.Session as Session
import qualified Station.Web.Tool as Web.Tool
import qualified Station.Web.SysOp as Web.SysOp
import qualified Station.Web.Learn as Web.Learn

handle_account :: Session.Type -> Wai.Application
handle_account Session.Record{Session.user = Just user} request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		let
			user_name = DB.User.name user
			xml = XML.element "account" [("name", user_name)] [Web.Tool.user_XML user]
			body = XML.xslt "account.xsl" xml
			in HTTP.respond_XML body request respond
handle_account session@Session.Record{Session.user = Just user} request respond
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
									(Session.database session)
							if ok
								then HTTP.respond_303 Constant.private_home request respond
								else HTTP.respond_400 "Failed to update password" request respond
				_ -> HTTP.respond_404 request respond
handle_account _ request respond =
	HTTP.respond_405 request respond

handle :: Session.Type -> Wai.Middleware
handle session next request respond =
	case Wai.pathInfo request of
		"sysop" : path -> Web.SysOp.handle session path next request respond
		["account"] -> handle_account session request respond
		"learn" : path -> Web.Learn.handle session path next request respond
		_ -> next request respond
