module Station.Web (handle) where

import Prelude ()
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Ord ((>))
import Data.Maybe (Maybe (Just))
import Data.Tuple (fst)
import Data.List (map, lookup)
import Data.Functor ((<$>))
import Control.Monad ((>>=))
import Text.Show (show)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse

import qualified Station.Constant as Constant
import qualified Station.XML as XML
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.Database.Subject as DB.Subject
import qualified Station.HTTP as HTTP
import qualified Station.Web.Session as Session
import qualified Station.Web.Bin as Web.Bin
import qualified Station.Web.SysOp as Web.SysOp
import qualified Station.Web.Learn as Web.Learn

handle_account :: DB.Type -> Wai.Application
handle_account db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		case HTTP.auth_user request of
			Just user_name' ->
				let
					user_name = BS.U8.toString user_name'
					xml = XML.element "account" [("name", user_name)] []
					body = XML.xslt "account.xsl" xml
					in HTTP.respond_XML body request respond
			_ -> HTTP.respond_403 request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (HTTP.auth_user request, lookup "password" parameters) of
				(Just username, Just password)
					| BS.length password > 0 ->
						do
							ok <- DB.User.set_password (BS.U8.toString username) (BS.U8.toString password) db
							if ok
								then HTTP.respond_303 Constant.private_home request respond
								else HTTP.respond_400 "Failed to update password" request respond
				_ -> HTTP.respond_404 request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_home :: DB.Type -> Wai.Application
handle_home db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		case HTTP.auth_user request of
			Just user_name' ->
				let user_name = BS.U8.toString user_name' in
					DB.User.get user_name db >>= \case
						[user] ->
							do
								subjects <- DB.Subject.list db
								let
									xml_subject subject =
										XML.element "subject" [] [
											XML.element "identifier" [] [XML.text (show (DB.Subject.identifier subject))],
											XML.element "title" [] [XML.text (DB.Subject.title subject)],
											XML.element "description" [] [XML.text (DB.Subject.description subject)]]
									xml =
										XML.element
											"home"
											[
												("name", user_name),
												("role", show (DB.User.role user))]
											[
												XML.element "subjects" [] (map xml_subject subjects)]
								HTTP.respond_XML (XML.xslt "/home.xsl" xml) request respond
						_ -> HTTP.respond_403 request respond
			_ -> HTTP.respond_404 request respond
	| otherwise =
		HTTP.respond_405 request respond

handle :: Session.Type -> Wai.Middleware
handle session next request respond =
	case Wai.pathInfo request of
		"bin" : path -> Web.Bin.handle path next request respond
		"sysop" : path -> Web.SysOp.handle session path next request respond
		["home"] -> handle_home (Session.database session) request respond
		["account"] -> handle_account (Session.database session) request respond
		"learn" : path -> Web.Learn.handle session path next request respond
		_ -> next request respond
