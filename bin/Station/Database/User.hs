{-# LANGUAGE QuasiQuotes #-}

module Station.Database.User (
	Type (Record, name, password, role, mark, lock, avatar),
	check_password, get_avatar, delete, hash_password, add, set, set_password
) where

import Prelude (fromEnum, toEnum)
import Data.Bool (Bool, not, (&&))
import Data.Eq ((==))
import Data.Maybe (Maybe (Nothing, Just))
import Data.List (filter)
import Data.String (String)
import Data.Functor (fmap, (<$>))
import Control.Applicative ((<*>))
import Control.Monad (return)
import System.IO (IO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.C8
import qualified Crypto.Scrypt
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Station.Constant as Constant
import qualified Station.Database
import qualified Station.Database.User.Add as Add
import qualified Station.Database.User.Set as Set

data Type =
	Record{
		name :: String,
		password :: String,
		role :: Constant.Role,
		mark :: Station.Database.Mark,
		lock :: Bool,
		avatar :: Maybe BS.ByteString}

instance DB.ToRow Type where
	toRow user =
		[
			DB.toField (name user),
			DB.toField (password user),
			DB.toField (fromEnum (role user)),
			DB.toField (mark user),
			DB.toField (lock user),
			DB.toField (DB.Binary <$> avatar user)]

instance DB.FromRow Type where
	fromRow =
		Record
			<$> DB.field
			<*> DB.field
			<*> (toEnum <$> DB.field)
			<*> DB.field
			<*> DB.field
			<*> (fmap DB.fromBinary <$> DB.field)

check_password :: String -> BS.ByteString -> DB.Connection -> IO [Type]
check_password user_name word db =
	fmap
		(filter
			(\ user ->
				(&&)
					(not (lock user))
					(Crypto.Scrypt.verifyPass'
						(Crypto.Scrypt.Pass word)
						(Crypto.Scrypt.EncryptedPass (BS.C8.pack (password user))))))
		(DB.query
			db
			[sql| SELECT "NAME","PASSWORD","ROLE","MARK","LOCK","AVATAR" FROM "USER" WHERE "NAME"=? |]
			(DB.Only user_name))

get_avatar :: String -> DB.Connection -> IO [BS.ByteString]
get_avatar user_name db =
	do
		result <- DB.query db [sql| SELECT "AVATAR" FROM "USER" WHERE "NAME"=? |] (DB.Only user_name)
		return (DB.fromBinary <$> DB.fromOnly <$> result)

delete :: String -> DB.Connection -> IO Bool
delete user db = fmap (1 ==) (DB.execute db [sql| DELETE FROM "USER" WHERE "NAME"=? |] (DB.Only user))

hash_password :: String -> IO BS.ByteString
hash_password word =
	fmap
		Crypto.Scrypt.getEncryptedPass
		(Crypto.Scrypt.encryptPassIO' (Crypto.Scrypt.Pass (BS.C8.pack word)))

add :: Add.Type -> DB.Connection -> IO Bool
add user db =
	do
		hash <- hash_password (Add.password user)
		n <-
			DB.execute
				db
				[sql| INSERT INTO "USER" ("NAME","PASSWORD","ROLE","MARK","LOCK","AVATAR") VALUES (?,?,?,?,?,?) |]
				(Add.name user, hash, fromEnum (Add.role user), Add.mark user, Add.lock user, Add.avatar user)
		return (n == 1)

set :: String -> Set.Type -> DB.Connection -> IO Bool
set old new db =
	case Set.password new of
		Nothing ->
			do
				n <-
					DB.execute
						db
						[sql| UPDATE "USER" SET "NAME"=?,"ROLE"=?,"MARK"=?,"LOCK"=? WHERE "NAME"=? |]
						(Set.name new, fromEnum (Set.role new), Set.mark new, Set.lock new, old)
				return (n == 1)
		Just new_password ->
			do
				hash <- hash_password new_password
				n <-
					DB.execute
						db
						[sql| UPDATE "USER" SET "NAME"=?,"PASSWORD"=?,"ROLE"=?,"LOCK"=? WHERE "NAME"=? |]
						(Set.name new, hash, fromEnum (Set.role new), Set.lock new, old)
				return (n == 1)

set_password :: String -> String -> DB.Connection -> IO Bool
set_password user_name new_password db =
	do
		hash <- hash_password new_password
		n <-
			DB.execute
				db
				[sql| UPDATE "USER" SET "PASSWORD"=? WHERE "NAME"=? |]
				(hash, user_name)
		return (n == 1)
