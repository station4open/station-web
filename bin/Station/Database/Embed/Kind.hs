module Station.Database.Embed.Kind (png, jpeg, youtube) where

import Prelude ()

import qualified Station.Database.Embed as DB.Embed

png :: DB.Embed.Kind
png = DB.Embed.Kind 1

jpeg :: DB.Embed.Kind
jpeg = DB.Embed.Kind 2

youtube :: DB.Embed.Kind
youtube = DB.Embed.Kind 101
