{-# LANGUAGE QuasiQuotes #-}

module
	Station.Database.Embed (
		Identifier, Number, Kind (Kind),
		kind_png, kind_jpeg, kind_youtube,
		Type (Record, identifier, lesson, number, title, kind, value),
		get, delete, add, set, exchange
	)
where

import Prelude ()
import Data.Bool (Bool (True, False))
import Data.Eq (Eq ((==), (/=)))
import Data.Maybe (Maybe (Nothing, Just))
import Control.Arrow (first)
import Data.List (map)
import Data.Int (Int16, Int32)
import Data.ByteString (ByteString)
import Data.String (String)
import Data.Functor (fmap, (<$>))
import Control.Applicative ((<*>))
import Control.Monad (return)
import Text.Show (Show, show)
import Text.Read (Read, readsPrec)
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB (ToField (toField))
import qualified Database.PostgreSQL.Simple.FromField as DB (FromField (fromField))
import qualified Database.PostgreSQL.Simple.ToRow as DB (ToRow (toRow))
import qualified Database.PostgreSQL.Simple.FromRow as DB (FromRow (fromRow), field)
import qualified Database.PostgreSQL.Simple.Transaction as DB (withTransactionSerializable)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Station.Database.Lesson as DB.Lesson

newtype Identifier = Identifier Int32

instance Show Identifier where
	show (Identifier i) = show i

instance Read Identifier where
	readsPrec i s = map (first Identifier) (readsPrec i s)

instance DB.ToField Identifier where
	toField (Identifier i) = DB.toField i

instance DB.FromField Identifier where
	fromField f x = Identifier <$> DB.fromField f x

type Number = Int16

newtype Kind = Kind Int32

instance Eq Kind where
	(Kind a) == (Kind b) = a == b
	(Kind a) /= (Kind b) = a /= b

instance Show Kind where
	show (Kind i) = show i

instance Read Kind where
	readsPrec i s = map (first Kind) (readsPrec i s)

instance DB.ToField Kind where
	toField (Kind i) = DB.toField i

instance DB.FromField Kind where
	fromField f x = Kind <$> DB.fromField f x

kind_png :: Kind
kind_png = Kind 1

kind_jpeg :: Kind
kind_jpeg = Kind 2

kind_youtube :: Kind
kind_youtube = Kind 101

data Type =
	Record{
		identifier :: Identifier,
		lesson :: DB.Lesson.Identifier,
		number :: Number,
		title :: String,
		kind :: Kind,
		value :: ByteString}

instance DB.ToRow Type where
	toRow embed =
		[
			DB.toField (identifier embed),
			DB.toField (lesson embed),
			DB.toField (number embed),
			DB.toField (title embed),
			DB.toField (kind embed),
			DB.toField (DB.Binary (value embed))]

instance DB.FromRow Type where
	fromRow =
		Record
			<$> DB.field
			<*> DB.field
			<*> DB.field
			<*> DB.field
			<*> DB.field
			<*> DB.field

get :: Identifier -> DB.Connection -> IO [Type]
get embed_identifier db =
	DB.query
		db
		[sql|
			SELECT "IDENTIFIER","LESSON","NUMBER","TITLE","KIND","VALUE"
			FROM "EMBED"
			WHERE "IDENTIFIER"=? |]
		(DB.Only embed_identifier)

delete :: Identifier -> DB.Connection -> IO Bool
delete embed_identifier db =
	DB.withTransactionSerializable
		db
		(do
			deleted <-
				DB.query
					db
					[sql| DELETE FROM "EMBED" WHERE "IDENTIFIER"=? RETURNING "LESSON" |]
					(DB.Only embed_identifier)
					:: IO [DB.Only DB.Lesson.Identifier]
			case deleted of
				DB.Only lesson_identifier : [] ->
					do
						_ <-
							DB.execute
								db
								[sql|
									WITH
										"E" AS (
											SELECT "IDENTIFIER", ROW_NUMBER() OVER(ORDER BY "NUMBER" ASC) AS "N"
											FROM "EMBED"
											WHERE "LESSON"=?
										)
									UPDATE "EMBED"
									SET "NUMBER"="E"."N"
									FROM "E"
									WHERE "EMBED"."IDENTIFIER"="E"."IDENTIFIER" AND "EMBED"."NUMBER"<>"E"."N" |]
								(DB.Only lesson_identifier)
						return True
				_ ->
					do
						DB.rollback db
						return False)

add :: DB.Lesson.Identifier -> String -> Kind -> ByteString -> DB.Connection -> IO (Maybe Identifier)
add embed_lesson embed_title embed_kind embed_value db =
	DB.withTransactionSerializable
		db
		(fmap
			(\case
				[DB.Only result] -> (Just result)
				_ -> Nothing)
			(DB.query
				db
				[sql|
					INSERT INTO "EMBED"("LESSON","NUMBER","TITLE","KIND","VALUE")
					VALUES (?, (SELECT COUNT(*)+1 FROM "EMBED" WHERE "LESSON"=?), ?, ?, ?)
					RETURNING "IDENTIFIER" |]
				(embed_lesson, embed_lesson, embed_title, embed_kind, (DB.Binary embed_value))))

set :: Identifier -> String -> Kind -> ByteString -> DB.Connection -> IO Bool
set embed_identifier embed_title embed_kind embed_value db =
	(1 ==)
		<$>
			DB.execute
				db
				[sql| UPDATE "EMBED" SET "TITLE"=?,"KIND"=?,"VALUE"=? WHERE "IDENTIFIER"=? |]
				(embed_title, embed_kind, embed_value, embed_identifier)

exchange :: Identifier -> Identifier -> DB.Connection -> IO Bool
exchange embed_1 embed_2 db =
	(2 ==)
		<$>
			DB.execute
				db
				[sql|
					WITH "E" AS (SELECT * FROM "EMBED" WHERE "IDENTIFIER" IN (?,?))
					UPDATE "EMBED"
					SET "NUMBER"="E2"."NUMBER"
					FROM "E" AS "E1", "E" AS "E2"
					WHERE "EMBED"."IDENTIFIER"="E1"."IDENTIFIER" AND "E1"."IDENTIFIER"<>"E2"."IDENTIFIER" |]
				(embed_1, embed_2)
