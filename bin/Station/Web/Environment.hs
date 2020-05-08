module Station.Web.Environment (Type (Record, log, database, user)) where

import Prelude ()
import Data.Bool (Bool)
import Data.Maybe (Maybe)
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User

data Type =
	Record{
		log :: Bool,
		database :: DB.Type,
		user :: Maybe DB.User.Type}
