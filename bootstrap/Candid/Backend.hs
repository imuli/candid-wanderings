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
  define :: t -> H.Hash -> Expression Int -> t
  exec :: t -> Expression Int -> t
  render :: t -> String

depends :: Store -> [H.Hash] -> Expression Int -> [H.Hash]
depends store = (%)
  where
    hs % (Pi _ _ inType outType) = hs % inType % outType
    hs % (Lambda _ _ inType body) = hs % inType % body
    hs % (Apply _ function argument) = hs % function % argument
    hs % (Assert _ bodyType body) = hs % bodyType % body
    hs % (Hash _ hsh) =
      if elem hsh hs
         then hs
         else case byHash store hsh of
                   Nothing -> hsh : hs
                   Just entry -> hsh : (hs % expr entry)
    hs % _ = hs

defFrom :: Backend t => Store -> t -> H.Hash -> t
defFrom store backend hsh = case byHash store hsh of
                                 Nothing -> backend
                                 Just entry -> define backend hsh (expr entry)

compile :: Backend t => t -> Store -> [Expression Int] -> String
compile backend store exprs = 
  let needs = reverse $ foldl (depends store) [] exprs
      withNeeds = foldl (defFrom store) backend needs
   in render $ foldl exec withNeeds exprs
