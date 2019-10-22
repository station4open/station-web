module Station.Web.SysOp (handle) where

import Prelude ()
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Maybe (Maybe (Just))
import Control.Monad ((>>=))
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.Text (Text)
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai

import qualified Station.XML as XML
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.HTTP as HTTP

account_GET :: DB.User.Type -> DB.Type -> Wai.Application
account_GET user _ request respond =
	do
		let
			xml = XML.element "account" [("name", DB.User.name user)] []
			body = XML.xslt "/sysop/account.xsl" xml
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

handle :: [Data.Text.Text] -> DB.Type -> Wai.Middleware
handle path db next =
	case path of
		["account.xml"] -> handle_account db
		_ -> next
