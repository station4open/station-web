module Station.Constant.Role (Role (SysOp, User)) where

import Prelude (Enum (toEnum, fromEnum), undefined)

data Role = SysOp | User

instance Enum Role where
	toEnum 0 = SysOp
	toEnum 1 = User
	toEnum _ = undefined
	fromEnum SysOp = 0
	fromEnum User  = 1
