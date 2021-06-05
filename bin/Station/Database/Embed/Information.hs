{-# LANGUAGE QuasiQuotes #-}

module
	Station.Database.Embed.Information (
		Type (Record, identifier, lesson, number, title, kind),
		list
	)
where

import Prelude ()
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.String (String)
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Station.Database.Lesson as DB.Lesson
import qualified Station.Database.Embed as DB.Embed

data Type =
	Record{
		identifier :: DB.Embed.Identifier,
		lesson :: DB.Lesson.Identifier,
		number :: DB.Embed.Number,
		title :: String,
		kind :: DB.Embed.Kind}

instance DB.ToRow Type where
	toRow embed =
		[
			DB.toField (identifier embed),
			DB.toField (lesson embed),
			DB.toField (number embed),
			DB.toField (title embed),
			DB.toField (kind embed)]

instance DB.FromRow Type where
	fromRow =
		Record
			<$> DB.field
			<*> DB.field
			<*> DB.field
			<*> DB.field
			<*> DB.field

list :: DB.Lesson.Identifier -> DB.Connection -> IO [Type]
list lesson_identifier db =
	DB.query
		db
		[sql|
			SELECT "IDENTIFIER","LESSON","NUMBER","TITLE","KIND"
			FROM "EMBED"
			WHERE "LESSON"=?
			ORDER BY "NUMBER" ASC |]
		(DB.Only lesson_identifier)
