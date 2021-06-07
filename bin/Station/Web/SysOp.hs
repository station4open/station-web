module Station.Web.SysOp (handle) where

import Prelude ()
import Data.Bool (Bool (True, False), otherwise)
import Data.Eq ((==))
import Data.Maybe (Maybe (Nothing, Just), isJust, fromJust)
import Data.Tuple (fst)
import Data.Monoid ((<>))
import Data.List (map, lookup)
import Data.Function (id, (.), flip)
import Data.Functor ((<$>))
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Data.ByteString.Lazy as BS.L
import Data.String (IsString)
import Control.Monad ((>>=), (=<<))
import Text.Show (show)
import Text.Read (readMaybe)
import qualified Data.Text (Text, unpack)
import qualified Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai.Parse

import qualified Station.Constant.Role as Constant.Role
import qualified Station.XML as XML
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User
import qualified Station.Database.User.Information as DB.User.Information
import qualified Station.Database.User.Add as DB.User.Add
import qualified Station.Database.User.Set as DB.User.Set
import qualified Station.Database.Subject as DB.Subject
import qualified Station.Database.Course as DB.Course
import qualified Station.Database.Lesson as DB.Lesson
import qualified Station.Database.Embed as DB.Embed
import qualified Station.Database.Embed.Information as DB.Embed.Information
import qualified Station.Database.Question as DB.Question
import qualified Station.Database.Answer as DB.Answer
import qualified Station.HTTP as HTTP
import qualified Station.Web.Tool as Web.Tool
import qualified Station.Web.Environment as Environment

path_prefix :: IsString s => s
path_prefix = "/sysop/"

handle_account :: Environment.Type -> Wai.Application
handle_account session request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		do
			users <- DB.User.Information.list (Environment.database session)
			HTTP.respond_XML
				(XML.xslt
					(path_prefix <> "account.xsl")
					(XML.element "account" [] (
						Web.Tool.user_XML (fromJust (Environment.user session)) :
						map
							(\ user ->
								XML.element
									"user"
									((if DB.User.Information.lock user then (("lock", "") :) else id)
										[("role", show (DB.User.Information.role user)), ("mark", show (DB.User.Information.mark user))])
									[XML.text (DB.User.Information.name user)])
							users)))
				request
				respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["delete", "user", "name", "role", "password", "mark", "lock"] of
				[Nothing, Nothing, Just name, Just role', Just password, Nothing, Nothing] ->
					{- create account -}
					case readMaybe (BS.U8.toString role') of
						Just role ->
							let new =
								DB.User.Add.Record{
									DB.User.Add.name = BS.U8.toString name,
									DB.User.Add.role = role,
									DB.User.Add.password = BS.U8.toString password,
									DB.User.Add.mark = 0,
									DB.User.Add.lock = False,
									DB.User.Add.avatar = Nothing}
								in redirected =<< DB.User.add new (Environment.database session)
						_ ->
							do
								BS.C8.putStr "Incorrect role: "
								BS.C8.putStrLn role'
								HTTP.respond_404 request respond
				[Nothing, Just user, Just name, Just role', Just password, Just mark', lock] ->
					{- modify account -}
					case (readMaybe (BS.U8.toString role'), readMaybe (BS.U8.toString mark')) of
						(Just role, Just mark) ->
							let new =
								DB.User.Set.Record{
									DB.User.Set.name = BS.U8.toString name,
									DB.User.Set.role = role,
									DB.User.Set.password =
										case password of
											"" -> Nothing
											_ -> Just (BS.U8.toString password),
									DB.User.Set.mark = mark,
									DB.User.Set.lock = isJust lock,
									DB.User.Set.avatar = Nothing}
								in redirected =<< DB.User.set (BS.U8.toString user) new (Environment.database session)
						_ ->
							do
								BS.C8.putStr "Incorrect role: "
								BS.C8.putStrLn role'
								HTTP.respond_404 request respond
				Just _ : Just user : _ ->
					{- delete account -}
					redirected =<< DB.User.delete (BS.U8.toString user) (Environment.database session)
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond
	where
		redirected True = HTTP.respond_303 (path_prefix <> "account") request respond
		redirected False = HTTP.respond_404 request respond

handle_subjects :: Environment.Type -> Wai.Application
handle_subjects session request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		do
			subjects <- DB.Subject.list (Environment.database session)
			HTTP.respond_XML
				(XML.xslt
					(path_prefix <> "subjects.xsl")
					(XML.element "subjects" [] (
						Web.Tool.user_XML (fromJust (Environment.user session)) :
						map
							(\ subject ->
								XML.element "item" [] [
									XML.element "identifier" [] [XML.text (show (DB.Subject.identifier subject))],
									XML.element "title" [] [XML.text (DB.Subject.title subject)],
									XML.element "description" [] [XML.text (DB.Subject.description subject)]])
							subjects)))
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
						DB.Subject.add title description (Environment.database session) >>= \case
							Nothing ->
								HTTP.respond_404 request respond
							Just identifier ->
								HTTP.respond_303 ("subject/" <> BS.U8.fromString (show identifier)) request respond
					_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_subject :: Environment.Type -> DB.Subject.Identifier -> Wai.Application
handle_subject session identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		DB.Subject.get identifier (Environment.database session) >>= \case
			[subject] ->
				do
					courses <- DB.Course.list identifier (Environment.database session)
					HTTP.respond_XML
						(XML.xslt
							(path_prefix <> "subject.xsl")
							(XML.element "subject" [] [
								Web.Tool.user_XML (fromJust (Environment.user session)),
								XML.element "identifier" [] [XML.text (show identifier)],
								XML.element "title" [] [XML.text (DB.Subject.title subject)],
								XML.element "description" [] [XML.text (DB.Subject.description subject)],
								XML.element "courses" []
									(map
										(\ course ->
											XML.element "course" [] [
												XML.element "identifier" [] [XML.text (show (DB.Course.identifier course))],
												XML.element "title" [] [XML.text (DB.Course.title course)],
												XML.element "description" [] [XML.text (DB.Course.description course)]])
										courses)]))
						request
						respond
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
							_ <- DB.Subject.delete identifier (Environment.database session)
							HTTP.respond_303 "../subjects" request respond
					(Just title, Just description, Nothing) ->
						let
							subject =
								DB.Subject.Record{
									DB.Subject.identifier = identifier,
									DB.Subject.title = title,
									DB.Subject.description = description}
							redirected True = HTTP.respond_303 (Wai.rawPathInfo request) request respond
							redirected False = HTTP.respond_404 request respond
							in redirected =<< DB.Subject.set subject (Environment.database session)
					_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_course_new :: DB.Type -> DB.Subject.Identifier -> Wai.Application
handle_course_new db subject request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["title", "description"] of
				[Just title, Just description] ->
					DB.Course.add subject (BS.U8.toString title) (BS.U8.toString description) db >>= \case
						Nothing ->
							HTTP.respond_404 request respond
						Just identifier ->
							HTTP.respond_303 ("../" <> BS.U8.fromString (show identifier)) request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_course :: Environment.Type -> DB.Course.Identifier -> Wai.Application
handle_course session identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		DB.Course.get identifier (Environment.database session) >>= \case
			[course] ->
				do
					lessons <- DB.Lesson.list identifier (Environment.database session)
					HTTP.respond_XML
						(XML.xslt
							(path_prefix <> "course.xsl")
							(XML.element "course" [] [
								Web.Tool.user_XML (fromJust (Environment.user session)),
								XML.element "identifier" [] [XML.text (show identifier)],
								XML.element "subject" [] [XML.text (show (DB.Course.subject course))],
								XML.element "title" [] [XML.text (DB.Course.title course)],
								XML.element "description" [] [XML.text (DB.Course.description course)],
								XML.element "lessons" []
									(map
										(\ (lesson_identifier, lesson_number, lesson_title) ->
											XML.element "lesson" [] [
												XML.element "identifier" [] [XML.text (show lesson_identifier)],
												XML.element "number" [] [XML.text (show lesson_number)],
												XML.element "title" [] [XML.text lesson_title]])
										lessons)]))
						request
						respond
			_ -> HTTP.respond_400 "Incorrect identifier" request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["subject", "title", "description", "delete"] of
				[Just subject, _, _, Just _] ->
					do
						_ <- DB.Course.delete identifier (Environment.database session)
						HTTP.respond_303 ("../subject/" <> subject) request respond
				[Just subject', Just title, Just description, Nothing] ->
					case readMaybe (BS.U8.toString subject') of
						Just subject ->
							let
								new =
									DB.Course.Record{
										DB.Course.identifier = identifier,
										DB.Course.subject = subject,
										DB.Course.title = BS.U8.toString title,
										DB.Course.description = BS.U8.toString description}
								in
									DB.Course.set new (Environment.database session) >>= \case
										True -> HTTP.respond_303 (Wai.rawPathInfo request) request respond
										False -> HTTP.respond_404 request respond
						_ -> HTTP.respond_400 "Incorrect subject" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_lesson_new :: DB.Type -> DB.Course.Identifier -> Wai.Application
handle_lesson_new db course request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (lookup "title" parameters, lookup "content" parameters) of
				(Just title, Just content) ->
					DB.Lesson.add course (BS.U8.toString title) (BS.U8.toString content) db >>= \case
						Nothing ->
							HTTP.respond_404 request respond
						Just identifier ->
							HTTP.respond_303 ("../" <> BS.U8.fromString (show identifier)) request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_lesson_exchange :: DB.Type -> DB.Course.Identifier -> Wai.Application
handle_lesson_exchange db course request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (lookup "0" parameters, lookup "1" parameters) of
				(Just a', Just b') ->
					case (readMaybe (BS.U8.toString a'), readMaybe (BS.U8.toString b')) of
						(Just a, Just b) ->
							DB.Lesson.exchange a b db >>= \case
								True -> HTTP.respond_303 ("../../course/" <> BS.U8.fromString (show course)) request respond
								False -> HTTP.respond_409 "Fail to re-order" request respond
						_ -> HTTP.respond_400 "Incorrect identifiers" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_lesson :: Environment.Type -> DB.Lesson.Identifier -> Wai.Application
handle_lesson session identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		DB.Lesson.get identifier (Environment.database session) >>= \case
			[lesson] ->
				do
					embeds <- DB.Embed.Information.list identifier (Environment.database session)
					questions <- DB.Question.list identifier (Environment.database session)
					HTTP.respond_XML
						(XML.xslt
							(path_prefix <> "lesson.xsl")
							(XML.element "lesson" [] [
								Web.Tool.user_XML (fromJust (Environment.user session)),
								XML.element "identifier" [] [XML.text (show identifier)],
								XML.element "number" [] [XML.text (show (DB.Lesson.number lesson))],
								XML.element "course" [] [XML.text (show (DB.Lesson.course lesson))],
								XML.element "title" [] [XML.text (DB.Lesson.title lesson)],
								XML.element "content" [] [XML.text (DB.Lesson.content lesson)],
								XML.element "embeds" []
									(map
										(\ embed ->
											XML.element "embed" [] [
												XML.element "identifier" [] [XML.text (show (DB.Embed.Information.identifier embed))],
												XML.element "number" [] [XML.text (show (DB.Embed.Information.number embed))],
												XML.element "title" [] [XML.text (DB.Embed.Information.title embed)],
												XML.element "kind" [] [XML.text (show (DB.Embed.Information.kind embed))]])
										embeds),
								XML.element "questions" []
									(map
										(\ question ->
											XML.element "question" [] [
												XML.element "identifier" [] [XML.text (show (DB.Question.identifier question))],
												XML.element "number" [] [XML.text (show (DB.Question.number question))],
												XML.element "text" [] [XML.text (DB.Question.text question)]])
										questions)]))
						request
						respond
			_ -> HTTP.respond_400 "Incorrect identifier" request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["course", "number", "title", "content", "delete"] of
				[Just course, _, _, _, Just _] ->
					do
						_ <- DB.Lesson.delete identifier (Environment.database session)
						HTTP.respond_303 ("../course/" <> course) request respond
				[Just course', Just number', Just title, Just content, Nothing] ->
					case (readMaybe (BS.U8.toString course'), readMaybe (BS.U8.toString number')) of
						(Just course, Just number) ->
							let
								new =
									DB.Lesson.Record{
										DB.Lesson.identifier = identifier,
										DB.Lesson.course = course,
										DB.Lesson.number = number,
										DB.Lesson.title = BS.U8.toString title,
										DB.Lesson.content = BS.U8.toString content}
								in
									DB.Lesson.set new (Environment.database session) >>= \case
										True -> HTTP.respond_303 (Wai.rawPathInfo request) request respond
										False -> HTTP.respond_409 "Fail to modify" request respond
						_ -> HTTP.respond_400 "Incorrect course or number" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_embed_new :: DB.Type -> DB.Lesson.Identifier -> Wai.Application
handle_embed_new db lesson request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			(parameters, files) <- Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (lookup "title" parameters, lookup "file" files, lookup "youtube" parameters) of
				(Just title, Just file, _) ->
					let
						add kind =
							let
								title' = BS.U8.toString title
								file' = BS.L.toStrict (Wai.Parse.fileContent file)
								url = "../../lesson/" <> BS.U8.fromString (show lesson)
								in
								DB.Embed.add lesson title' kind file' db >>= \case
									Nothing -> HTTP.respond_404 request respond
									_ -> HTTP.respond_303 url request respond
						in
						case Wai.Parse.fileContentType file of
							"image/png" -> add DB.Embed.kind_png
							"image/jpeg" -> add DB.Embed.kind_jpeg
							_ -> HTTP.respond_400 "Incorrect file type" request respond
				(Just title, _, Just youtube) ->
					-- TODO
					HTTP.respond_500 "TODO" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_embed :: DB.Type -> DB.Embed.Identifier -> Wai.Application
handle_embed db identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			let respond_ok lesson = HTTP.respond_303 ("../lesson/" <> lesson) request respond
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["lesson", "delete", "exchange", "title"] of
				[Just lesson, Just _, _, _] ->
					do
						_ <- DB.Embed.delete identifier db
						respond_ok lesson
				[Just lesson, Nothing, Just exchange', _] ->
					case readMaybe (BS.U8.toString exchange') of
						Just exchange ->
							DB.Embed.exchange identifier exchange db >>= \case
								True -> respond_ok lesson
								False -> HTTP.respond_409 "Fail to re-order" request respond
						_ -> HTTP.respond_400 "Incorrect identifiers" request respond
				[Just lesson, Nothing, _, Just title] ->
					DB.Embed.set_title identifier (BS.U8.toString title) db >>= \case
						True -> respond_ok lesson
						False -> HTTP.respond_409 "Fail to modify" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_question_new :: DB.Type -> DB.Lesson.Identifier -> Wai.Application
handle_question_new db lesson request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case lookup "text" parameters of
				Just text ->
					DB.Question.add lesson (BS.U8.toString text) db >>= \case
						Nothing ->
							HTTP.respond_404 request respond
						Just identifier ->
							HTTP.respond_303 ("../" <> BS.U8.fromString (show identifier)) request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_question_exchange :: DB.Type -> DB.Question.Identifier -> Wai.Application
handle_question_exchange db lesson request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (lookup "0" parameters, lookup "1" parameters) of
				(Just a', Just b') ->
					case (readMaybe (BS.U8.toString a'), readMaybe (BS.U8.toString b')) of
						(Just a, Just b) ->
							DB.Question.exchange a b db >>= \case
								True -> HTTP.respond_303 ("../../lesson/" <> BS.U8.fromString (show lesson)) request respond
								False -> HTTP.respond_409 "Fail to re-order" request respond
						_ -> HTTP.respond_400 "Incorrect identifiers" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_question :: Environment.Type -> DB.Question.Identifier -> Wai.Application
handle_question session identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodGet =
		DB.Question.get identifier (Environment.database session) >>= \case
			[question] ->
				do
					answers <- DB.Answer.list identifier (Environment.database session)
					HTTP.respond_XML
						(XML.xslt
							(path_prefix <> "question.xsl")
							(XML.element "question" [] [
								Web.Tool.user_XML (fromJust (Environment.user session)),
								XML.element "identifier" [] [XML.text (show identifier)],
								XML.element "lesson" [] [XML.text (show (DB.Question.lesson question))],
								XML.element "number" [] [XML.text (show (DB.Question.number question))],
								XML.element "text" [] [XML.text (DB.Question.text question)],
								XML.element "answers" []
									(map
										(\ answer ->
											XML.element "answer" [] [
												XML.element "identifier" [] [XML.text (show (DB.Answer.identifier answer))],
												XML.element "text" [] [XML.text (DB.Answer.text answer)],
												XML.element "mark" [] [XML.text (show (DB.Answer.mark answer))]])
										answers)]))
						request
						respond
			_ -> HTTP.respond_400 "Incorrect identifier" request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["lesson", "text", "delete"] of
				[Just lesson, _, Just _] ->
					do
						_ <- DB.Question.delete identifier (Environment.database session)
						HTTP.respond_303 ("../lesson/" <> lesson) request respond
				[_, Just text, Nothing] ->
					DB.Question.set identifier (BS.U8.toString text) (Environment.database session) >>= \case
						True -> HTTP.respond_303 (Wai.rawPathInfo request) request respond
						False -> HTTP.respond_409 "Fail to modify" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_answer_new :: DB.Type -> DB.Question.Identifier -> Wai.Application
handle_answer_new db question request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case (lookup "text" parameters, (readMaybe . BS.U8.toString) =<< lookup "mark" parameters) of
				(Just text, Just mark) ->
					DB.Answer.add question (BS.U8.toString text) mark db >>= \case
						Nothing ->
							HTTP.respond_404 request respond
						Just _ ->
							HTTP.respond_303 ("../../question/" <> BS.U8.fromString (show question)) request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle_answer :: Environment.Type -> DB.Answer.Identifier -> Wai.Application
handle_answer session identifier request respond
	| Wai.requestMethod request == Network.HTTP.Types.methodPost =
		do
			parameters <- fst <$> Wai.Parse.parseRequestBody Wai.Parse.lbsBackEnd request
			case map (flip lookup parameters) ["question", "text", "mark", "delete"] of
				[Just question, _, _, Just _] ->
					do
						_ <- DB.Answer.delete identifier (Environment.database session)
						HTTP.respond_303 ("../question/" <> question) request respond
				[Just question', Just text, Just mark', Nothing] ->
					case (readMaybe (BS.U8.toString question'), readMaybe (BS.U8.toString mark')) of
						(Just question, Just mark) ->
							let
								new =
									DB.Answer.Record{
										DB.Answer.identifier = identifier,
										DB.Answer.question = question,
										DB.Answer.text = BS.U8.toString text,
										DB.Answer.mark = mark}
								in
									DB.Answer.set new (Environment.database session) >>= \case
										True -> HTTP.respond_303 ("../question/" <> question') request respond
										False -> HTTP.respond_404 request respond
						_ -> HTTP.respond_400 "Incorrect question" request respond
				_ -> HTTP.respond_400 "Incorrect form field" request respond
	| otherwise =
		HTTP.respond_405 request respond

handle :: Environment.Type -> [Data.Text.Text] -> Wai.Middleware
handle session path next request respond =
	case session of
		Environment.Record{
			Environment.user =
				Just DB.User.Record{
					DB.User.role = Constant.Role.SysOp},
			Environment.database = db}
			->
				case path of
					["account"] -> handle_account session request respond
					["subjects"] -> handle_subjects session request respond
					["subject", subject'] ->
						case readMaybe (Data.Text.unpack subject') of
							Just subject -> handle_subject session subject request respond
							_ -> next request respond
					["course", "new", subject'] ->
						case readMaybe (Data.Text.unpack subject') of
							Just subject -> handle_course_new db subject request respond
							_ -> next request respond
					["course", course'] ->
						case readMaybe (Data.Text.unpack course') of
							Just course -> handle_course session course request respond
							_ -> next request respond
					["lesson", "new", course'] ->
						case readMaybe (Data.Text.unpack course') of
							Just course -> handle_lesson_new db course request respond
							_ -> next request respond
					["lesson", "exchange", course'] ->
						case readMaybe (Data.Text.unpack course') of
							Just course -> handle_lesson_exchange db course request respond
							_ -> next request respond
					["lesson", lesson'] ->
						case readMaybe (Data.Text.unpack lesson') of
							Just lesson -> handle_lesson session lesson request respond
							_ -> next request respond
					["embed", "new", lesson'] ->
						case readMaybe (Data.Text.unpack lesson') of
							Just lesson -> handle_embed_new db lesson request respond
							_ -> next request respond
					["embed", embed'] ->
						case readMaybe (Data.Text.unpack embed') of
							Just embed -> handle_embed db embed request respond
							_ -> next request respond
					["question", "new", lesson'] ->
						case readMaybe (Data.Text.unpack lesson') of
							Just lesson -> handle_question_new db lesson request respond
							_ -> next request respond
					["question", "exchange", lesson'] ->
						case readMaybe (Data.Text.unpack lesson') of
							Just lesson -> handle_question_exchange db lesson request respond
							_ -> next request respond
					["question", question'] ->
						case readMaybe (Data.Text.unpack question') of
							Just question -> handle_question session question request respond
							_ -> next request respond
					["answer", "new", question'] ->
						case readMaybe (Data.Text.unpack question') of
							Just question -> handle_answer_new db question request respond
							_ -> next request respond
					["answer", answer'] ->
						case readMaybe (Data.Text.unpack answer') of
							Just answer -> handle_answer session answer request respond
							_ -> next request respond
					_ -> next request respond
		_ -> HTTP.respond_403 request respond
