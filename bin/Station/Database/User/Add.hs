{-# LANGUAGE QuasiQuotes #-}

module Station.Database.User.Add (Type (Record, name, password, role, mark, lock, avatar)) where

import Prelude (fromEnum, toEnum)
import Data.Bool (Bool)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

import qualified Station.Constant
import qualified Station.Database

data Type =
	Record{
		name :: String,
		password :: String,
		role :: Station.Constant.Role,
		mark :: Station.Database.Mark,
		lock :: Bool,
		avatar :: Maybe BS.ByteString}

instance DB.ToRow Type where
	toRow record =
		[
			DB.toField (name record),
			DB.toField (password record),
			DB.toField (fromEnum (role record)),
			DB.toField (mark record),
			DB.toField (lock record),
			DB.toField (avatar record)]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field <*> (toEnum <$> DB.field) <*> DB.field <*> DB.field <*> DB.field
