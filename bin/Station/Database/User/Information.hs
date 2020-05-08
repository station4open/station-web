{-# LANGUAGE QuasiQuotes #-}

module Station.Database.User.Information (
	Type (Record, name, password, role, mark, lock, avatar),
	get, list
) where

import Prelude (toEnum)
import Data.Bool (Bool)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import System.IO (IO)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Station.Constant
import qualified Station.Database

data Type =
	Record{
		name :: String,
		password :: String,
		role :: Station.Constant.Role,
		mark :: Station.Database.Mark,
		lock :: Bool,
		avatar :: Bool}

instance DB.FromRow Type where
	fromRow =
		Record
			<$> DB.field
			<*> DB.field
			<*> (toEnum <$> DB.field)
			<*> DB.field
			<*> DB.field
			<*> DB.field

select :: DB.Query
select = [sql| SELECT "NAME","PASSWORD","ROLE","MARK","LOCK","AVATAR" IS NOT NULL FROM "USER" |]

get :: String -> DB.Connection -> IO [Type]
get user_name db = DB.query db (select <> [sql| WHERE "NAME"=? |]) (DB.Only user_name)

list :: DB.Connection -> IO [Type]
list db = DB.query_ db select
