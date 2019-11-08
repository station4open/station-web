module Station.Database.Subject (
	Identifier,
	BodyType (Body, title, description),
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

type Identifier = Int32

data BodyType =
	Body{
		title :: String,
		description :: String}

instance DB.ToRow BodyType where
	toRow subject = [DB.toField (title subject), DB.toField (description subject)]

instance DB.FromRow BodyType where
	fromRow = Body <$> DB.field <*> DB.field

data Type =
	Record{
		identifier :: Identifier,
		body :: BodyType}

instance DB.ToRow Type where
	toRow subject = DB.toField (identifier subject) : DB.toRow (body subject)

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.fromRow

get :: Identifier -> DB.Connection -> IO [BodyType]
get subject_identifier db =
	DB.query
		db
		"SELECT \"TITLE\",\"DESCRIPTION\" FROM \"SUBJECT\" WHERE \"IDENTIFIER\"=?"
		(DB.Only subject_identifier)

list :: DB.Connection -> IO [Type]
list db = DB.query_ db "SELECT \"IDENTIFIER\",\"TITLE\",\"DESCRIPTION\" FROM \"SUBJECT\""

delete :: Identifier -> DB.Connection -> IO Bool
delete subject_identifier db =
	(1 ==)
		<$>
			DB.execute
				db
				"DELETE FROM \"SUBJECT\" WHERE \"IDENTIFIER\"=?"
				(DB.Only subject_identifier)

add :: BodyType -> DB.Connection -> IO (Maybe Int32)
add subject db =
	(\case
		[DB.Only result] -> (Just result)
		_ -> Nothing)
		<$>
			DB.query
				db
				"INSERT INTO \"SUBJECT\"(\"TITLE\",\"DESCRIPTION\") VALUES (?,?) RETURNING \"IDENTIFIER\""
				(title subject, description subject)

set :: Type -> DB.Connection -> IO Bool
set subject db =
	(1 ==)
		<$>
			DB.execute
				db
				"UPDATE \"SUBJECT\" SET \"TITLE\"=?,\"DESCRIPTION\"=? WHERE \"IDENTIFIER\"=?"
				(title (body subject), description (body subject), identifier subject)
