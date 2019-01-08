

{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables
           , TypeOperators
           , ViewPatterns
           #-}

-- |
-- Module      :  Data.HtsSet
-- Copyright   :  (c) Zoltan Kelemen 2017
-- License     :  BSD-style
-- Maintainer  :  kelemzol@elte.hu
--
-- HtsSet is a Heterogenous Set wich can provide storing values with different type.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.HtsSet as HSet

module Data.HtsSet ( HtsSet
                   , empty, singleton
                   , null, size, member, notMember
                   , existTypeOf, existTypeOfP, existTypeOfP'
                   , appl, compliance
                   , insert
                   , lookup, lookupWithDefault
                   , update
                   , deleteByType, deleteByTypeP, deleteByTypeP', deleteWhen
                   , (:+) (..), Append (..), fill
                   , Proxy(..)
                   ) where

import qualified Data.Map as M
import Data.Typeable

import Prelude hiding (lookup, null)

data CastBox = forall a. (Typeable a) => CastBox { unBox :: a }

newtype HtsSet = HtsSet { unHS :: M.Map TypeRep CastBox }


mapCastBox :: forall a. Typeable a => (a -> a) -> CastBox -> CastBox
mapCastBox f o@(CastBox e) = case cast e of
    (Just e') -> CastBox (f e')
    Nothing -> o

-- | The empty HtsSet
empty :: HtsSet
empty = HtsSet M.empty

-- | A HtsSet with an element
singleton :: forall a. Typeable a => a -> HtsSet
singleton a = HtsSet (M.singleton (typeRep (Proxy :: Proxy a)) (CastBox a))

-- | Is the HtsSet is empty?
--
-- > null empty == True
-- > null (singleton "a") == False
null :: HtsSet -> Bool
null = M.null . unHS

-- | The number of elements in the HtsSet
--
-- > size empty == 0
-- > size (singleton "a") == 1
size :: HtsSet -> Int
size = M.size . unHS

-- | The HtsSet is contain an element?
--
-- > member (Proxy :: Proxy String) empty == False
-- > member (Proxy :: Proxy String) (singleton "a") == True
member :: forall proxy a. (Typeable a, Eq a) => a -> HtsSet -> Bool
member elem (HtsSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just (CastBox (cast -> Just elem'))) -> elem == elem'
    _ -> False


-- | The HtsSet is not contain an element?
notMember :: forall proxy a. (Typeable a, Eq a) => a -> HtsSet -> Bool
notMember elem (HtsSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just (CastBox (cast -> Just elem'))) -> elem /= elem'
    _ -> True

-- | The HtsSet is contain a same type of element?
--
-- > let hs = insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
-- > existTypeOf "string" hs == True
existTypeOf :: forall a. Typeable a => a -> HtsSet -> Bool
existTypeOf _ (HtsSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just _) -> True
    _ -> False

-- | The HtsSet is contain a same type of element? (by proxy)
existTypeOfP :: forall proxy a. Typeable a => proxy a -> HtsSet -> Bool
existTypeOfP _ (HtsSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just _) -> True
    _ -> False

-- | The HtsSet is contain a same type of element? (by fixed proxy)
existTypeOfP' :: forall a. Typeable a => Proxy a -> HtsSet -> Bool
existTypeOfP' _ (HtsSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just _) -> True
    _ -> False

-- | Apply a function to an element with a default value
--
-- > appl "no ABC" (:"BC") $ singleton 'A' == "ABC"
-- > appl "no ABC" (:"BC") $ singleton "s" == "no ABC"
appl :: forall a b. Typeable a => b -> (a -> b) -> HtsSet -> b
appl def fn hs = case lookup hs of
    Nothing -> def
    (Just a) -> fn a

-- | appl specialization
compliance :: forall a. Typeable a => Bool -> (a -> Bool) -> HtsSet -> Bool
compliance = appl

-- | Insert a new value in the HtsSet. If the a elem is already present in the HtsSet with type, the associated value is replaced with the supplied value
--
-- > insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
insert :: forall a. Typeable a => a -> HtsSet -> HtsSet
insert a (HtsSet hs) = HtsSet (M.insert (typeRep (Proxy :: Proxy a)) (CastBox a) hs)

-- | Lookup a value from in the HtsSet
--
-- > let hs = insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
-- > lookup hs == Just "a"
-- > lookup hs == Just (2 :: Int)
-- > but
-- > lookup hs == Just 2 -- is False! Because the type of 2 is Num t => t not Int
lookup :: forall a. Typeable a => HtsSet -> Maybe a
lookup (HtsSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just (CastBox a)) -> cast a
    _ -> Nothing

-- | Lookup a value from in the HtsSet with a default value
lookupWithDefault :: forall a. Typeable a => a -> HtsSet -> a
lookupWithDefault a hs = case lookup hs of
    Nothing -> a
    (Just a') -> a'

-- | Update a value in HtsSet
--
-- > let hs = insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
-- > let hs' = update (++"b") hs
-- > lookup hs' == Just "ab"
update :: forall a. Typeable a => (a -> a) -> HtsSet -> HtsSet
update f = HtsSet . M.adjust (mapCastBox f) (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by type
--
-- > (member 'c' $ deleteByType 'b' $ singleton 'c') == False
deleteByType :: forall a. Typeable a => a -> HtsSet -> HtsSet
deleteByType _ = HtsSet . M.delete (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by type (by proxy)
--
-- > (member 'c' $ deleteByTypeP (Proxy :: Proxy Char) $ singleton 'c') == False
deleteByTypeP :: forall proxy a. Typeable a => proxy a -> HtsSet -> HtsSet
deleteByTypeP _ = HtsSet . M.delete (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by type (by fixed proxy)
deleteByTypeP' :: forall a. Typeable a => Proxy a -> HtsSet -> HtsSet
deleteByTypeP' _ = HtsSet . M.delete (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by condition
deleteWhen :: forall a. Typeable a => (a -> Bool) -> HtsSet -> HtsSet
deleteWhen cond hs = case lookup hs of
    Nothing -> hs
    (Just a) -> if cond a then deleteByType a hs else hs

-- | Helper heterogeneous list for comfortable HtsSet building (with append and fill)
--
-- > let hs = fill ("a" :+ 'c' :+ True :+ ())
-- > lookup hs == Just 'c'
-- > use () to close the list
-- > lookup hs == Just () -- is False!
-- > let hs' = fill ("a" :+ 'c' :+ True :+ () :+ ())
-- > lookup hs' == Just () -- is Ok
data a :+ b = a :+ b
infixr 5 :+

class Append a where
    append :: a -> HtsSet -> HtsSet

fill :: (Append a) => a -> HtsSet
fill = flip append empty

instance (Typeable a, Append b) => Append (a :+ b) where
    append (a :+ b) = insert a . (append b)

instance Append () where
    append _ hs = hs
