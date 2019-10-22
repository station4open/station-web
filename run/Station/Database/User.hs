module Station.Database.User (
	Type (Record, name, password, role),
	get, set_password, set_role
) where

import Prelude (fromEnum, toEnum)
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.String (String)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (return)
import System.IO (IO)
import qualified Data.ByteString as BS
import qualified Crypto.Scrypt
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB

import qualified Station.Constant as Constant

data Type =
	Record {
		name :: String,
		password :: String,
		role :: Constant.Role}

instance DB.ToRow Type where
	toRow user = [DB.toField (name user), DB.toField (password user), DB.toField (fromEnum (role user))]

instance DB.FromRow Type where
	fromRow = Record <$> DB.field <*> DB.field <*> (toEnum <$> DB.field)

get :: String -> DB.Connection -> IO [Type]
get user_name db = DB.query db "SELECT \"NAME\",\"PASSWORD\",\"ROLE\" FROM \"USER\" WHERE \"NAME\"=?" (DB.Only user_name)

set_password :: String -> BS.ByteString -> DB.Connection -> IO Bool
set_password user_name new_password db =
	do
		encrypted <- Crypto.Scrypt.encryptPassIO' (Crypto.Scrypt.Pass new_password)
		n <-
			DB.execute
				db
				"UPDATE \"USER\" SET \"PASSWORD\"=? WHERE \"NAME\"=?"
				(Crypto.Scrypt.getEncryptedPass encrypted, user_name)
		return (n == 1)

set_role :: String -> Constant.Role -> DB.Connection -> IO Bool
set_role user_name new_role db =
	do
		n <-
			DB.execute
				db
				"UPDATE \"USER\" SET \"ROLE\"=? WHERE \"NAME\"=?"
				(fromEnum new_role, user_name)
		return (n == 1)
