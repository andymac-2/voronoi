module ReferenceList (
  Ref,
  RefData,
  withRefData,
  insert,
  adjust,
  delete,
  (!)
  ) where

import qualified Data.HashMap.Lazy as H

newtype Ref = Ref Int

data RefData v = RefData Int (H.HashMap Int v)

withRefData :: (RefData v -> a) -> a
withRefData f = f (RefData 0 H.empty)

insert :: v -> RefData v -> (Ref, RefData v)
insert v (RefData uid hashmap) = 
  (Ref uid, RefData (uid + 1) (H.insert uid v hashmap))
  
adjust :: (v -> v) -> Ref -> RefData v -> RefData v
adjust f (Ref k) (RefData uid hashmap) =
  RefData uid (H.adjust f k hashmap)

delete :: Ref -> RefData v -> RefData v 
delete (Ref k) (RefData uid hashmap) = RefData uid (H.delete k hashmap)

(!) :: RefData v -> Ref -> v
(!) (RefData uid hashmap) (Ref key) = hashmap H.! key