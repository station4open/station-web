module Station.Database.User (
	Type (Record, name, password, role),
	get, list, check, delete, add, set, set_password
) where

import Prelude (fromEnum, toEnum)
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.List (any)
import Data.String (String)
import Data.Function ((.))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (return)
import System.IO (IO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS.U8
import qualified Crypto.Scrypt
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

import qualified Station.Constant as Constant

data Type =
	Record{
		name :: String,
		password :: String,
		role :: Constant.Role}

instance DB.ToRow Type where
	toRow user = [DB.toField (name user), DB.toField (password user), DB.toField (fromEnum (role user))]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field <*> (toEnum <$> DB.field)

get :: String -> DB.Connection -> IO [Type]
get user_name db = DB.query db "SELECT \"NAME\",\"PASSWORD\",\"ROLE\" FROM \"USER\" WHERE \"NAME\"=?" (DB.Only user_name)

list :: DB.Connection -> IO [Type]
list db = DB.query_ db "SELECT \"NAME\",\"PASSWORD\",\"ROLE\" FROM \"USER\""

check :: BS.ByteString -> BS.ByteString -> DB.Connection -> IO Bool
check user word db =
	do
		hashed <- DB.query db "SELECT \"PASSWORD\" FROM \"USER\" WHERE \"NAME\"=?" (DB.Only user)
		let
			pass :: Crypto.Scrypt.Pass
			pass = Crypto.Scrypt.Pass word
		return (any (Crypto.Scrypt.verifyPass' pass . Crypto.Scrypt.EncryptedPass . DB.fromOnly) hashed)

hash_password :: String -> IO BS.ByteString
hash_password word =
	Crypto.Scrypt.getEncryptedPass <$> Crypto.Scrypt.encryptPassIO' (Crypto.Scrypt.Pass (BS.U8.fromString word))

delete :: String -> DB.Connection -> IO Bool
delete user db = (1 ==) <$> DB.execute db "DELETE FROM \"USER\" WHERE \"NAME\"=?" (DB.Only user)

add :: Type -> DB.Connection -> IO Bool
add new_user db =
	do
		hash <- hash_password (password new_user)
		n <-
			DB.execute
				db
				"INSERT INTO \"USER\" (\"NAME\", \"PASSWORD\", \"ROLE\") VALUES (?, ?, ?)"
				(name new_user, hash, fromEnum (role new_user))
		return (n == 1)

set :: String -> Type -> DB.Connection -> IO Bool
set old_name new_user db =
	case password new_user of
		"" ->
			do
				n <-
					DB.execute
						db
						"UPDATE \"USER\" SET \"NAME\"=?, \"ROLE\"=? WHERE \"NAME\"=?"
						(name new_user, fromEnum (role new_user), old_name)
				return (n == 1)
		new_password ->
			do
				hash <- hash_password new_password
				n <-
					DB.execute
						db
						"UPDATE \"USER\" SET \"NAME\"=?, \"PASSWORD\"=?, \"ROLE\"=? WHERE \"NAME\"=?"
						(name new_user, hash, fromEnum (role new_user), old_name)
				return (n == 1)

set_password :: String -> String -> DB.Connection -> IO Bool
set_password user_name new_password db =
	do
		hash <- hash_password new_password
		n <-
			DB.execute
				db
				"UPDATE \"USER\" SET \"PASSWORD\"=? WHERE \"NAME\"=?"
				(hash, user_name)
		return (n == 1)
