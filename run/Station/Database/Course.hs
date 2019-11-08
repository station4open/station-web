module Station.Database.Course (
	Identifier,
	BodyType (Body, subject, title, description),
	Type (Record, identifier, body),
	get, list, delete, add, set
) where

import Prelude ()
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Maybe (Maybe (Nothing, Just))
import Data.Int (Int32)
import Data.String (String)
import Control.Applicative ((<$>), (<*>))
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

import qualified Station.Database.Subject as DB.Subject

type Identifier = Int32

data BodyType =
	Body{
		subject :: DB.Subject.Identifier,
		title :: String,
		description :: String}

instance DB.ToRow BodyType where
	toRow course = [DB.toField (subject course), DB.toField (title course), DB.toField (description course)]

instance DB.FromRow BodyType where
	fromRow = Body <$> DB.field <*> DB.field <*> DB.field

data Type =
	Record{
		identifier :: Identifier,
		body :: BodyType}

instance DB.ToRow Type where
	toRow course = DB.toField (identifier course) : DB.toRow (body course)

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.fromRow

get :: Identifier -> DB.Connection -> IO [BodyType]
get course_identifier db =
	DB.query
		db
		"SELECT \"SUBJECT\",\"TITLE\",\"DESCRIPTION\" FROM \"COURSE\" WHERE \"IDENTIFIER\"=?"
		(DB.Only course_identifier)

list :: DB.Subject.Identifier -> DB.Connection -> IO [Type]
list subject_identifier db =
	DB.query
		db
		"SELECT \"IDENTIFIER\",\"SUBJECT\",\"TITLE\",\"DESCRIPTION\" FROM \"COURSE\" WHERE \"SUBJECT\"=?"
		(DB.Only subject_identifier)

delete :: Identifier -> DB.Connection -> IO Bool
delete course_identifier db =
	(1 ==)
		<$>
			DB.execute
				db
				"DELETE FROM \"COURSE\" WHERE \"IDENTIFIER\"=?"
				(DB.Only course_identifier)

add :: BodyType -> DB.Connection -> IO (Maybe Identifier)
add course db =
	(\case
		[DB.Only result] -> (Just result)
		_ -> Nothing)
		<$>
			DB.query
				db
				"INSERT INTO \"COURSE\"(\"SUBJECT\",\"TITLE\",\"DESCRIPTION\") VALUES (?,?,?) RETURNING \"IDENTIFIER\""
				(subject course, title course, description course)

set :: Type -> DB.Connection -> IO Bool
set course db =
	(1 ==)
		<$>
			DB.execute
				db
				"UPDATE \"COURSE\" SET \"SUBJECT\"=?,\"TITLE\"=?,\"DESCRIPTION\"=? WHERE \"IDENTIFIER\"=?"
				(subject (body course), title (body course), description (body course), identifier course)
