module Station.Web.Learn (handle) where

import Prelude ()
import Data.Maybe (Maybe (Just))
import Data.Monoid ((<>))
import Data.List (map)
import Data.String (IsString)
import Control.Monad ((>>=))
import Text.Show (show)
import Text.Read (readMaybe)
import qualified Data.Text (Text, unpack)
import qualified Network.Wai as Wai

import qualified Station.XML as XML
import qualified Station.Database.Subject as DB.Subject
import qualified Station.Database.Course as DB.Course
import qualified Station.HTTP as HTTP
import qualified Station.Web.Session as Session

path_prefix :: IsString s => s
path_prefix = "/learn/"

handle_subject :: Session.Type -> DB.Subject.Identifier -> Wai.Application
handle_subject session identifier request respond =
	DB.Subject.get identifier (Session.database session) >>= \case
		[subject] ->
			do
				courses <- DB.Course.list identifier (Session.database session)
				HTTP.respond_XML
					(XML.xslt
						(path_prefix <> "subject.xsl")
						(XML.element "subject" [] [
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

handle :: Session.Type -> [Data.Text.Text] -> Wai.Middleware
handle session path next request respond =
	case path of
		["subject", subject'] ->
			case readMaybe (Data.Text.unpack subject') of
				Just subject -> handle_subject session subject request respond
				_ -> HTTP.respond_404 request respond
		_ -> next request respond
