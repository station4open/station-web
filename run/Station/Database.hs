module Station.Database (Type) where

import Prelude ()
import qualified Database.PostgreSQL.Simple

type Type = Database.PostgreSQL.Simple.Connection
