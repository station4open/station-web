module Station.Web.SysOp (handle) where

import Prelude ()
import Data.Bool (Bool (True, False), otherwise)
import Data.Eq ((==))
import Data.Maybe (Maybe (Nothing, Just))
import Data.Tuple (fst)
import Data.Monoid ((<>))
import Data.List (map, lookup)
import Data.String (IsString)
import Data.Function (flip)
import Data.Functor ((<$>))
import Control.Monad ((=<<))
import Text.Show (show)
import Text.Read (reads)
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.Text (Text)
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse

import qualified Station.XML as XML
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.HTTP as HTTP

path_prefix :: IsString s => s
path_prefix = "/sysop/"

handle_account :: DB.Type -> Wai.Application
handle_account db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		do
			users <- DB.User.list db
			let
				xml_user user = XML.element "user" [("role", show (DB.User.role user))] [XML.text (DB.User.name user)]
				xml_account = XML.element "account" [] (map xml_user users)
				body = XML.xslt (path_prefix <> "account.xsl") xml_account
			HTTP.respond_XML body request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["user", "name", "role", "password", "delete"] of
				[Nothing, Just name, Just role', Just password, Nothing] ->
					case reads (BS.U8.toString role') of
						(role, "") : _ ->
							let new =
								DB.User.Record{
									DB.User.name = BS.U8.toString name,
									DB.User.role = role,
									DB.User.password = BS.U8.toString password}
								in
								redirect_result =<< DB.User.add new db
						_ ->
							do
								BS.C8.putStr "Incorrect role: "
								BS.C8.putStrLn role'
								HTTP.respond_404 request respond
				[Just user, Just name, Just role', Just password, Nothing] ->
					case reads (BS.U8.toString role') of
						(role, "") : _ ->
							let new =
								DB.User.Record{
									DB.User.name = BS.U8.toString name,
									DB.User.role = role,
									DB.User.password = BS.U8.toString password}
								in
								redirect_result =<< DB.User.set (BS.U8.toString user) new db
						_ ->
							do
								BS.C8.putStr "Incorrect role: "
								BS.C8.putStrLn role'
								HTTP.respond_404 request respond
				[Just user, _, _, _, Just _] ->
					redirect_result =<< DB.User.delete (BS.U8.toString user) db
				_ -> HTTP.respond_409 "Unknown form field" request respond
	| otherwise =
		HTTP.respond_405 request respond
	where
		redirect_result True = HTTP.respond_303 (path_prefix <> "account.xml") request respond
		redirect_result False = HTTP.respond_404 request respond

handle :: [Data.Text.Text] -> DB.Type -> Wai.Middleware
handle path db next request respond =
	case path of
		["account.xml"] -> handle_account db request respond
		_ -> next request respond
