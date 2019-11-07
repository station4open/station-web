module Station.Database.Course (
	Type (Record, subject, title, description),
	get, list, delete, add, set
) where

import Prelude ()
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.String (String)
import Control.Applicative ((<$>), (<*>))
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

data Type =
	Record{
		subject :: String,
		title :: String,
		description :: String}

instance DB.ToRow Type where
	toRow course = [DB.toField (subject course), DB.toField (title course), DB.toField (description course)]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field <*> DB.field

get :: String -> String -> DB.Connection -> IO [Type]
get subject' title' db =
	DB.query
		db
		"SELECT \"SUBJECT\",\"TITLE\",\"DESCRIPTION\" FROM \"COURSE\" WHERE (\"SUBJECT\",\"TITLE\")=(?,?)"
		(subject', title')

list :: String -> DB.Connection -> IO [Type]
list subject' db =
	DB.query
		db
		"SELECT \"SUBJECT\",\"TITLE\",\"DESCRIPTION\" FROM \"COURSE\" WHERE \"SUBJECT\"=?"
		(DB.Only subject')

delete :: String -> String -> DB.Connection -> IO Bool
delete subject' title' db =
	(1 ==)
		<$>
			DB.execute
				db
				"DELETE FROM \"COURSE\" WHERE (\"SUBJECT\",\"TITLE\")=(?,?)"
				(subject', title')

add :: Type -> DB.Connection -> IO Bool
add course db =
	(1 ==)
		<$>
			DB.execute
				db
				"INSERT INTO \"COURSE\" (\"SUBJECT\",\"TITLE\", \"DESCRIPTION\") VALUES (?,?,?)"
				(subject course, title course, description course)

set :: String -> String -> Type -> DB.Connection -> IO Bool
set old_subject old_title new db =
	(1 ==)
		<$>
			DB.execute
				db
				"UPDATE \"COURSE\" SET \"SUBJECT\"=?, \"TITLE\"=?, \"DESCRIPTION\"=? WHERE (\"SUBJECT\", \"TITLE\")=(?,?)"
				(subject new, title new, description new, old_subject, old_title)
