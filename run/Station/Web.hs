module Station.Web (handle) where

import Prelude ()
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Maybe (Maybe (Just))
import Control.Monad ((>>=))
import qualified Data.ByteString.Char8 as BS.C8
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai

import qualified Station.XML as XML
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.HTTP as HTTP
import qualified Station.Web.Bin as Web.Bin
import qualified Station.Web.SysOp as Web.SysOp

account_GET :: DB.User.Type -> DB.Type -> Wai.Application
account_GET user _ request respond =
	do
		let
			xml = XML.element "account" [("name", DB.User.name user)] []
			body = XML.xslt "/account.xsl" xml
		HTTP.respond_XML body request respond

handle_account :: DB.Type -> Wai.Application
handle_account db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		case HTTP.auth_user request of
			Just name ->
				DB.User.get (BS.C8.unpack name) db >>= \case
					[user] -> account_GET user db request respond
					_ -> HTTP.respond_404 request respond
			_ -> HTTP.respond_404 request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		HTTP.respond_404 request respond
	| otherwise =
		HTTP.respond_404 request respond

handle_home :: DB.Type -> Wai.Application
handle_home _ request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		case HTTP.auth_user request of
			Just name ->
				do
					let
						xml = XML.element "home" [("name", BS.C8.unpack name)] []
						body = XML.xslt "/home.xsl" xml
					HTTP.respond_XML body request respond
			_ -> HTTP.respond_404 request respond
	| otherwise =
		HTTP.respond_404 request respond

handle :: DB.Type -> Wai.Middleware
handle db next request respond =
	case Wai.pathInfo request of
		"bin" : path -> Web.Bin.handle path db next request respond
		"sysop" : path -> Web.SysOp.handle path db next request respond
		["home.xml"] -> handle_home db request respond
		["account.xml"] -> handle_account db request respond
		_ -> next request respond
