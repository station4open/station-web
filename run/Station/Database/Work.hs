module Station.Database.Work (
	Type (Record, user, answer),
	get, delete
) where

import Prelude ()
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Maybe (Maybe (Nothing, Just))
import Data.String (String)
import Control.Applicative ((<$>), (<*>))
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

import qualified Station.Database.Question as DB.Question
import qualified Station.Database.Answer as DB.Answer

data Type =
	Record{
		user :: String,
		answer :: DB.Answer.Identifier}

instance DB.ToRow Type where
	toRow work =
		[
			DB.toField (user work),
			DB.toField (answer work)]

instance DB.FromRow Type where
	fromRow =
		Record
			<$> DB.field
			<*> DB.field

get :: String -> DB.Question.Identifier -> DB.Connection -> IO [Type]
get user_name question_identifier db =
	DB.query
		db
		"SELECT \"WORK\".\"USER\",\"WORK\".\"ANSWER\" \
			\INNER JOIN \"ANSWER\" \
				\ON \"ANSWER\".\"IDENTIFIER\"=\"WORK\".\"ANSWER\" \
			\FROM \"WORK\" \
			\WHERE \"WORK\".\"USER\"=? AND \"ANSWER\".\"QUESTION\"=?"
		(user_name, question_identifier)

delete :: String -> DB.Question.Identifier -> DB.Connection -> IO Bool
delete user_name question_identifier db =
	(1 ==)
		<$>
			DB.execute
				db
				"DELETE \"WORK\" \
					\USING \"QUESTION\",\"ANSWER\" \
					\WHERE \"WORK\".\"USER\"=? \
						\AND \"ANSWER\".\"QUESTION\"=? \
						\AND \"ANSWER\".\"IDENTIFIER\"=\"WORK\".\"ANSWER\""
				(user_name, question_identifier)
