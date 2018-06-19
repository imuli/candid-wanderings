module Candid.Backend
  ( Backend(..)
  , compile
  , depends
  ) where

import Candid.Expression
import Candid.Store
import qualified Blake2s1 as H

class Backend t where
  empty :: t
  define :: t -> H.Hash -> Expression -> t
  exec :: t -> Expression -> t
  render :: t -> String

depends :: Store -> [H.Hash] -> Expression -> [H.Hash]
depends store = (%)
  where
    hs % (Name _ body) = hs % body
    hs % (Pi _ inType outType) = hs % inType % outType
    hs % (Lambda _ inType body) = hs % inType % body
    hs % (Apply function argument) = hs % function % argument
    hs % (Assert bodyType body) = hs % bodyType % body
    hs % (Hash _ hsh) =
      if elem hsh hs
         then hs
         else case byHash store hsh of
                   Nothing -> hsh : hs
                   Just entry -> hsh : (hs % entryExpr entry)
    hs % _ = hs

defFrom :: Backend t => Store -> t -> H.Hash -> t
defFrom store backend hsh = case byHash store hsh of
                                 Nothing -> backend
                                 Just entry -> define backend hsh (entryExpr entry)

compile :: Backend t => t -> Store -> [Expression] -> String
compile backend store exprs = 
  let needs = reverse $ foldl (depends store) [] exprs
      withNeeds = foldl (defFrom store) backend needs
   in render $ foldl exec withNeeds exprs
