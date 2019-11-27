module Station.Database.Answer (
	Identifier, Mark,
	Type (Record, identifier, question, text, mark),
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

import qualified Station.Database.Question as DB.Question

type Identifier = Int32
type Mark = Int32

data Type =
	Record{
		identifier :: Identifier,
		question :: DB.Question.Identifier,
		text :: String,
		mark :: Mark}

instance DB.ToRow Type where
	toRow answer =
		[
			DB.toField (identifier answer),
			DB.toField (question answer),
			DB.toField (text answer),
			DB.toField (mark answer)]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field <*> DB.field <*> DB.field

get :: Identifier -> DB.Connection -> IO [Type]
get answer_identifier db =
	DB.query
		db
		"SELECT \"IDENTIFIER\",\"QUESTION\",\"TEXT\",\"MARK\" FROM \"ANSWER\" WHERE \"IDENTIFIER\"=?"
		(DB.Only answer_identifier)

list :: DB.Question.Identifier -> DB.Connection -> IO [Type]
list question_identifier db =
	DB.query
		db
		"SELECT \"IDENTIFIER\",\"QUESTION\",\"TEXT\",\"MARK\" FROM \"ANSWER\" WHERE \"QUESTION\"=?"
		(DB.Only question_identifier)

delete :: Identifier -> DB.Connection -> IO Bool
delete answer_identifier db =
	(1 ==)
		<$>
			DB.execute
				db
				"DELETE FROM \"ANSWER\" WHERE \"IDENTIFIER\"=?"
				(DB.Only answer_identifier)

add :: (DB.Question.Identifier, String, Mark) -> DB.Connection -> IO (Maybe Identifier)
add (answer_question, answer_text, answer_mark) db =
	(\case
		[DB.Only result] -> (Just result)
		_ -> Nothing)
		<$>
			DB.query
				db
				"INSERT INTO \"ANSWER\"(\"QUESTION\",\"TEXT\",\"MARK\") VALUES (?,?,?) RETURNING \"IDENTIFIER\""
				(answer_question, answer_text, answer_mark)

set :: Type -> DB.Connection -> IO Bool
set answer db =
	(1 ==)
		<$>
			DB.execute
				db
				"UPDATE \"ANSWER\" SET \"QUESTION\"=?,\"TEXT\"=?,\"MARK\"=? WHERE \"IDENTIFIER\"=?"
				(question answer, text answer, mark answer, identifier answer)
