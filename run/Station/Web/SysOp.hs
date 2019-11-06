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
import System.IO (print)
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.Text (Text)
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse

import qualified Station.XML as XML
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.Database.Subject as DB.Subject
import qualified Station.Database.Course as DB.Course
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
								in redirect_result =<< DB.User.add new db
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
								in redirect_result =<< DB.User.set (BS.U8.toString user) new db
						_ ->
							do
								BS.C8.putStr "Incorrect role: "
								BS.C8.putStrLn role'
								HTTP.respond_404 request respond
				[Just user, _, _, _, Just _] ->
					redirect_result =<< DB.User.delete (BS.U8.toString user) db
				_ -> HTTP.respond_422 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond
	where
		redirect_result True = HTTP.respond_303 (path_prefix <> "account.xml") request respond
		redirect_result False = HTTP.respond_404 request respond

handle_subject :: DB.Type -> Wai.Application
handle_subject db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		do
			subjects <- DB.Subject.list db
			let
				xml_item subject =
					XML.element
						"item"
						[]
						[
							XML.element "title" [] [XML.text (DB.Subject.title subject)],
							XML.element "description" [] [XML.text (DB.Subject.description subject)]]
				xml_subject = XML.element "subjects" [] (map xml_item subjects)
				body = XML.xslt (path_prefix <> "subject.xsl") xml_subject
			HTTP.respond_XML body request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["subject", "title", "description", "delete"] of
				[Nothing, Just title, Just description, Nothing] ->
					let new =
						DB.Subject.Record{
							DB.Subject.title = BS.U8.toString title,
							DB.Subject.description = BS.U8.toString description}
						in redirect_result =<< DB.Subject.add new db
				[Just subject, Just title, Just description, Nothing] ->
					let new =
						DB.Subject.Record{
							DB.Subject.title = BS.U8.toString title,
							DB.Subject.description = BS.U8.toString description}
						in redirect_result =<< DB.Subject.set (BS.U8.toString subject) new db
				[Just subject, _, _, Just _] ->
					redirect_result =<< DB.Subject.delete (BS.U8.toString subject) db
				[Just subject_title', Nothing, Nothing, Nothing] ->
					do
						let subject_title = BS.U8.toString subject_title'
						subjects <- DB.Subject.get subject_title db
						case subjects of
							[subject] ->
								do
									courses <- DB.Course.list subject_title db
									let
										xml_course course =
											XML.element
												"course"
												[]
												[
													XML.element "title" [] [XML.text (DB.Course.title course)],
													XML.element "description" [] [XML.text (DB.Course.description course)]]
										xml =
											XML.element
												"subject"
												[]
												[
													XML.element "title" [] [XML.text (DB.Subject.title subject)],
													XML.element "description" [] [XML.text (DB.Subject.description subject)],
													XML.element "courses" [] (map xml_course courses)]
									HTTP.respond_XML (XML.xslt (path_prefix <> "subject.xsl") xml) request respond
							_ -> HTTP.respond_404 request respond
				x ->
					do
						print x
						HTTP.respond_422 "DEBUG: Incorrect form field" request respond
				--	_ -> HTTP.respond_422 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond
	where
		redirect_result True = HTTP.respond_303 (path_prefix <> "subject.xml") request respond
		redirect_result False = HTTP.respond_404 request respond

handle_course :: DB.Type -> Wai.Application
handle_course db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		HTTP.respond_405 request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["subject", "course", "title", "description", "delete"] of
				[Just subject_title', Just course_title', Nothing, Nothing, Nothing] ->
					do
						courses <-
							let
								subject_title = BS.U8.toString subject_title'
								course_title = BS.U8.toString course_title'
								in DB.Course.get subject_title course_title db
						case courses of
							[course] ->
								let
									xml =
										XML.element
											"course"
											[]
											[
												XML.element "subject" [] [XML.text (DB.Course.subject course)],
												XML.element "title" [] [XML.text (DB.Course.title course)],
												XML.element "description" [] [XML.text (DB.Course.description course)]]
									in HTTP.respond_XML (XML.xslt (path_prefix <> "course.xsl") xml) request respond
							_ -> HTTP.respond_404 request respond
				_ -> HTTP.respond_422 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle :: [Data.Text.Text] -> DB.Type -> Wai.Middleware
handle path db next request respond =
	case path of
		["account.xml"] -> handle_account db request respond
		["subject.xml"] -> handle_subject db request respond
		["course.xml"] -> handle_course db request respond
		_ -> next request respond
