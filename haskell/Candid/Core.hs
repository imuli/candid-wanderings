{-#OPTIONS_GHC -Wall #-}

module Candid.Core
  ( reduce
  , Expr(..)
  , pretty
  , readExpr
  , readExprL
  , TypeError(..)
  , typeOf
  , Store
  , empty
  , find
  , findName
  , entryExpr
  , entryName
  , entryType
  , add
  , unhash
  , hashOf
  , hashWith
  , hashInto
  , Hash(..)
  , Hashable(..)
  ) where

import Candid.Expr
import Candid.Reduce
import Candid.TypeCheck
import Candid.Store
import Candid.Hash

