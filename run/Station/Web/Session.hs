module Station.Web.Session (Type (Record, database, user)) where

import Prelude ()
import Data.Maybe (Maybe)
import qualified Station.Database as DB
import qualified Station.Database.User as DB.User

data Type =
	Record{
		database :: DB.Type,
		user :: Maybe DB.User.Type}
