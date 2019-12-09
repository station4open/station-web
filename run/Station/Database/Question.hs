module Station.Database.Question (
	Identifier,
	Type (Record, identifier, lesson, text),
	get, list, delete, add, set
) where

import Prelude ()
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Maybe (Maybe (Nothing, Just))
import Data.List (map)
import Data.Int (Int32)
import Data.String (String)
import Control.Applicative ((<$>), (<*>))
import Text.Show (Show, show)
import Text.Read (Read, readsPrec)
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.FromField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

import qualified Station.Database.Lesson as DB.Lesson

newtype Identifier = Identifier Int32

instance Show Identifier where
	show (Identifier i) = show i

instance Read Identifier where
	readsPrec i s = map (\ (x, r) -> (Identifier x, r)) (readsPrec i s)

instance DB.ToField Identifier where
	toField (Identifier i) = DB.toField i

instance DB.FromField Identifier where
	fromField f x = Identifier <$> DB.fromField f x

data Type =
	Record{
		identifier :: Identifier,
		lesson :: DB.Lesson.Identifier,
		text :: String}

instance DB.ToRow Type where
	toRow question =
		[
			DB.toField (identifier question),
			DB.toField (lesson question),
			DB.toField (text question)]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field <*> DB.field

get :: Identifier -> DB.Connection -> IO [Type]
get question_identifier db =
	DB.query
		db
		"SELECT \"IDENTIFIER\",\"LESSON\",\"TEXT\" FROM \"QUESTION\" WHERE \"IDENTIFIER\"=?"
		(DB.Only question_identifier)

list :: DB.Lesson.Identifier -> DB.Connection -> IO [Type]
list lesson_identifier db =
	DB.query
		db
		"SELECT \"IDENTIFIER\",\"LESSON\",\"TEXT\" FROM \"QUESTION\" WHERE \"LESSON\"=?"
		(DB.Only lesson_identifier)

delete :: Identifier -> DB.Connection -> IO Bool
delete question_identifier db =
	(1 ==)
		<$>
			DB.execute
				db
				"DELETE FROM \"QUESTION\" WHERE \"IDENTIFIER\"=?"
				(DB.Only question_identifier)

add :: DB.Lesson.Identifier -> String -> DB.Connection -> IO (Maybe Identifier)
add question_lesson question_text db =
	(\case
		[DB.Only result] -> (Just result)
		_ -> Nothing)
		<$>
			DB.query
				db
				"INSERT INTO \"QUESTION\"(\"LESSON\",\"TEXT\") VALUES (?,?) RETURNING \"IDENTIFIER\""
				(question_lesson, question_text)

set :: Type -> DB.Connection -> IO Bool
set question db =
	(1 ==)
		<$>
			DB.execute
				db
				"UPDATE \"QUESTION\" SET \"LESSON\"=?,\"TEXT\"=? WHERE \"IDENTIFIER\"=?"
				(lesson question, text question, identifier question)
