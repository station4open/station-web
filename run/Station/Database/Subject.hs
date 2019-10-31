module Station.Database.Subject (
	Type (Record, title, description),
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
		title :: String,
		description :: String}

instance DB.ToRow Type where
	toRow subject = [DB.toField (title subject), DB.toField (description subject)]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field

get :: String -> DB.Connection -> IO [Type]
get key db = DB.query db "SELECT \"TITLE\",\"DESCRIPTION\" FROM \"SUBJECT\" WHERE \"TITLE\"=?" (DB.Only key)

list :: DB.Connection -> IO [Type]
list db = DB.query_ db "SELECT \"TITLE\",\"DESCRIPTION\" FROM \"SUBJECT\""

delete :: String -> DB.Connection -> IO Bool
delete key db = (1 ==) <$> DB.execute db "DELETE FROM \"SUBJECT\" WHERE \"TITLE\"=?" (DB.Only key)

add :: Type -> DB.Connection -> IO Bool
add subject db =
	(1 ==)
		<$>
			DB.execute
				db
				"INSERT INTO \"SUBJECT\" (\"TITLE\", \"DESCRIPTION\") VALUES (?, ?)"
				(title subject, description subject)

set :: String -> Type -> DB.Connection -> IO Bool
set old new db =
	(1 ==)
		<$>
			DB.execute
				db
				"UPDATE \"SUBJECT\" SET \"TITLE\"=?, \"DESCRIPTION\"=? WHERE \"TITLE\"=?"
				(title new, description new, old)
