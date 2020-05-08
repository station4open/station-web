{-# LANGUAGE QuasiQuotes #-}

module Station.Database.User.Set (Type (Record, name, password, role, mark, lock, avatar)) where

import Prelude ()
import Data.Bool (Bool)
import Data.Maybe (Maybe)
import Data.String (String)
import qualified Data.ByteString as BS

import qualified Station.Constant
import qualified Station.Database

data Type =
	Record{
		name :: String,
		password :: Maybe String,
		role :: Station.Constant.Role,
		mark :: Station.Database.Mark,
		lock :: Bool,
		avatar :: Maybe (Maybe BS.ByteString)}
