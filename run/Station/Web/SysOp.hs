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
import Control.Monad ((>>=), (=<<))
import Text.Show (show)
import Text.Read (reads)
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.Text (Text, unpack)
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
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond
	where
		redirect_result True = HTTP.respond_303 (path_prefix <> "account.xml") request respond
		redirect_result False = HTTP.respond_404 request respond

handle_subjects :: DB.Type -> Wai.Application
handle_subjects db request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		do
			subjects <- DB.Subject.list db
			let
				xml_item subject =
					XML.element
						"item"
						[]
						[
							XML.element "identifier" [] [XML.text (show (DB.Subject.identifier subject))],
							XML.element "title" [] [XML.text (DB.Subject.title (DB.Subject.body subject))],
							XML.element "description" [] [XML.text (DB.Subject.description (DB.Subject.body subject))]]
			HTTP.respond_XML
				(XML.xslt
					(path_prefix <> "subjects.xsl")
					(XML.element "subjects" [] (map xml_item subjects)))
				request
				respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case
				(
					BS.U8.toString <$> lookup "title" parameters,
					BS.U8.toString <$> lookup "description" parameters)
				of
					(Just title, Just description) ->
						let
							subject =
								DB.Subject.Body{
									DB.Subject.title = title,
									DB.Subject.description = description}
							in
								DB.Subject.add subject db >>= \case
									Nothing ->
										HTTP.respond_404 request respond
									Just identifier ->
										HTTP.respond_303 ("subject/" <> BS.U8.fromString (show identifier)) request respond
					_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_subject :: DB.Type -> DB.Subject.Identifier -> Wai.Application
handle_subject db identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		DB.Subject.get identifier db >>= \case
			[subject] ->
				do
					courses <- DB.Course.list identifier db
					let
						build_course_XML course =
							XML.element
								"course"
								[]
								[
									XML.element "identifier" [] [XML.text (show (DB.Course.identifier course))],
									XML.element "title" [] [XML.text (DB.Course.title (DB.Course.body course))],
									XML.element "description" [] [XML.text (DB.Course.description (DB.Course.body course))]]
						courses_XML =
							XML.element "courses" [] (map build_course_XML courses)
						xml =
							XML.element
								"subject"
								[]
								[
									XML.element "identifier" [] [XML.text (show identifier)],
									XML.element "title" [] [XML.text (DB.Subject.title subject)],
									XML.element "description" [] [XML.text (DB.Subject.description subject)],
									courses_XML]
					HTTP.respond_XML (XML.xslt (path_prefix <> "subject.xsl") xml) request respond
			_ -> HTTP.respond_404 request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case
				(
					BS.U8.toString <$> lookup "title" parameters,
					BS.U8.toString <$> lookup "description" parameters,
					BS.U8.toString <$> lookup "delete" parameters)
				of
					(_, _, Just _) ->
						do
							_ <- DB.Subject.delete identifier db
							HTTP.respond_303 "../subjects" request respond
					(Just title, Just description, Nothing) ->
						let
							subject =
								DB.Subject.Record{
									DB.Subject.identifier = identifier,
									DB.Subject.body =
										DB.Subject.Body{
											DB.Subject.title = title,
											DB.Subject.description = description}}
							redirect_result True = HTTP.respond_303 "" request respond
							redirect_result False = HTTP.respond_404 request respond
							in redirect_result =<< DB.Subject.set subject db
					_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_courses :: DB.Type -> DB.Subject.Identifier -> Wai.Application
handle_courses db subject request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["title", "description", "delete"] of
				[Just title, Just description, Nothing] ->
					let
						new =
							DB.Course.Body{
								DB.Course.subject = subject,
								DB.Course.title = BS.U8.toString title,
								DB.Course.description = BS.U8.toString description}
						in
							DB.Course.add new db >>= \case
								Nothing ->
									HTTP.respond_404 request respond
								Just identifier ->
									HTTP.respond_303 ("../course/" <> BS.U8.fromString (show identifier)) request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_course :: DB.Type -> DB.Course.Identifier -> Wai.Application
handle_course db identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		DB.Course.get identifier db >>= \case
			[course] ->
				HTTP.respond_XML
					(XML.xslt
						(path_prefix <> "course.xsl")
						(XML.element
							"course"
							[]
							[
								XML.element "subject" [] [XML.text (show (DB.Course.subject course))],
								XML.element "title" [] [XML.text (DB.Course.title course)],
								XML.element "description" [] [XML.text (DB.Course.description course)]]))
					request
					respond
			_ -> HTTP.respond_400 "Incorrect identifier" request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["subject", "title", "description", "delete"] of
				[Just subject, _, _, Just _] ->
					do
						_ <- DB.Course.delete identifier db
						HTTP.respond_303 ("../subject/" <> subject) request respond
				[Just subject', Just title, Just description, Nothing] ->
					case reads (BS.U8.toString subject') of
						((subject, _) : _) ->
							let
								new =
									DB.Course.Record{
										DB.Course.identifier = identifier,
										DB.Course.body =
											DB.Course.Body{
												DB.Course.subject = subject,
												DB.Course.title = BS.U8.toString title,
												DB.Course.description = BS.U8.toString description}}
								in
									DB.Course.set new db >>= \case
										True -> HTTP.respond_303 "" request respond
										False -> HTTP.respond_404 request respond
						_ -> HTTP.respond_400 "Incorrect subject" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle :: [Data.Text.Text] -> DB.Type -> Wai.Middleware
handle path db next request respond =
	case path of
		["account"] -> handle_account db request respond
		["subjects"] -> handle_subjects db request respond
		["subject", subject'] ->
			case reads (Data.Text.unpack subject') of
				((subject, _) : _) -> handle_subject db subject request respond
				_ -> next request respond
		["courses", subject'] ->
			case reads (Data.Text.unpack subject') of
				((subject, _) : _) -> handle_courses db subject request respond
				_ -> next request respond
		["course", course'] ->
			case reads (Data.Text.unpack course') of
				((course, _) : _) -> handle_course db course request respond
				_ -> next request respond
		_ -> next request respond
