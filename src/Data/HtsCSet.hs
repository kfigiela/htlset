

{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables
           , TypeOperators
           , ConstraintKinds
           , MultiParamTypeClasses
           , FlexibleInstances
           , ViewPatterns
           , PolyKinds
           #-}

-- |
-- Module      :  Data.HtsCSet
-- Copyright   :  (c) Zoltan Kelemen 2017
-- License     :  BSD-style
-- Maintainer  :  kelemzol@elte.hu
--
-- HtsCSet is a Heterogenous Set wich can provide storing values with different and constrained type.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.HtsCSet as HCSet

module Data.HtsCSet ( HtsCSet
                    , empty, emptyP, singleton, singletonP
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
import GHC.Exts (Constraint)

import Prelude hiding (lookup, null)

data CastBox c = forall a. (Typeable a, c a) => CastBox { unBox :: a }

newtype HtsCSet c = HtsCSet { unHS :: M.Map TypeRep (CastBox c) }

mapCastBox :: forall c a. (Typeable a, c a) => (a -> a) -> CastBox c -> CastBox c
mapCastBox f o@(CastBox e) = case cast e of
    (Just e') -> CastBox (f e')
    Nothing -> o

-- | The empty HtsCSet
empty :: HtsCSet c
empty = HtsCSet M.empty

-- | The empty HtsCSet with proxy
emptyP :: proxy c -> HtsCSet c
emptyP _ = empty

-- | A HtsCSet with an element
singleton :: forall c a. (Typeable a, c a) => a -> HtsCSet c
singleton a = HtsCSet (M.singleton (typeRep (Proxy :: Proxy a)) (CastBox a))

-- | A HtsCSet with an element with proxy
singletonP :: forall proxy c a. (Typeable a, c a) => proxy c -> a -> HtsCSet c
singletonP _ = singleton

-- | Is the HtsCSet is empty?
--
-- > null empty == True
-- > null (singleton "a") == False
null :: HtsCSet c -> Bool
null = M.null . unHS

-- | The number of elements in the HtsSet
--
-- > size empty == 0
-- > size (singleton "a") == 1
size :: HtsCSet c -> Int
size = M.size . unHS

-- | The HtsSet is contain an element?
--
-- > member (Proxy :: Proxy String) empty == False
-- > member (Proxy :: Proxy String) (singleton "a") == True
member :: forall proxy c a. (Typeable a, Eq a, c a) => a -> HtsCSet c -> Bool
member elem (HtsCSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just (CastBox (cast -> Just elem'))) -> elem == elem'
    _ -> False


-- | The HtsSet is not contain an element?
notMember :: forall proxy c a. (Typeable a, Eq a, c a) => a -> HtsCSet c -> Bool
notMember elem (HtsCSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just (CastBox (cast -> Just elem'))) -> elem /= elem'
    _ -> True

-- | The HtsCSet is contain a same type of element?
--
-- > let hs = insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
-- > existTypeOf "string" hs == True
existTypeOf :: forall c a. (Typeable a, c a) => a -> HtsCSet c -> Bool
existTypeOf _ (HtsCSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just _) -> True
    _ -> False

-- | The HtsCSet is contain a same type of element? (by proxy)
existTypeOfP :: forall proxy c a. (Typeable a, c a) => proxy a -> HtsCSet c -> Bool
existTypeOfP _ (HtsCSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just _) -> True
    _ -> False

-- | The HtsCSet is contain a same type of element? (by fixed proxy)
existTypeOfP' :: forall c a. (Typeable a, c a) => Proxy a -> HtsCSet c -> Bool
existTypeOfP' _ (HtsCSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just _) -> True
    _ -> False

-- | Apply a function to an element with a default value
--
-- > appl "no ABC" (:"BC") $ singleton 'A' == "ABC"
-- > appl "no ABC" (:"BC") $ singleton "s" == "no ABC"
appl :: forall a b c. (Typeable a, c a) => b -> (a -> b) -> HtsCSet c -> b
appl def fn hs = case lookup hs of
    Nothing -> def
    (Just a) -> fn a

-- | appl specialization
compliance :: forall a c. (Typeable a, c a) => Bool -> (a -> Bool) -> HtsCSet c -> Bool
compliance = appl

-- | Insert a new value in the HtsCSet. If the a elem is already present in the HtsCSet with type, the associated value is replaced with the supplied value
--
-- > insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
insert :: forall c a. (Typeable a, c a) => a -> HtsCSet c -> HtsCSet c
insert a (HtsCSet hs) = HtsCSet (M.insert (typeRep (Proxy :: Proxy a)) (CastBox a) hs)

-- | Lookup a value from in the HtsCSet
--
-- > let hs = insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
-- > lookup hs == Just "a"
-- > lookup hs == Just (2 :: Int)
-- > but
-- > lookup hs == Just 2 -- is False! Because the type of 2 is Num t => t not Int
lookup :: forall c a. (Typeable a, c a) => HtsCSet c -> Maybe a
lookup (HtsCSet hs) = case M.lookup (typeRep (Proxy :: Proxy a)) hs of
    (Just (CastBox a)) -> cast a
    _ -> Nothing

-- | Lookup a value from in the HtsCSet with a default value
lookupWithDefault :: forall c a. (Typeable a, c a) => a -> HtsCSet c -> a
lookupWithDefault a hs = case lookup hs of
    Nothing -> a
    (Just a') -> a'

-- | Update a value in HtsCSet
--
-- > let hs = insert "a" $ insert (2 :: Int) $ insert 'c' $ empty
-- > let hs' = update (++"b") hs
-- > lookup hs' == Just "ab"
update :: forall c a. (Typeable a, c a) => (a -> a) -> HtsCSet c -> HtsCSet c
update f = HtsCSet . M.adjust (mapCastBox f) (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by type
--
-- > (member 'c' $ deleteByType 'b' $ singleton 'c') == False
deleteByType :: forall a c. (Typeable a, c a) => a -> HtsCSet c -> HtsCSet c
deleteByType _ = HtsCSet . M.delete (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by type (by proxy)
--
-- > (member 'c' $ deleteByTypeP (Proxy :: Proxy Char) $ singleton 'c') == False
deleteByTypeP :: forall proxy c a. (Typeable a, c a) => proxy a -> HtsCSet c -> HtsCSet c
deleteByTypeP _ = HtsCSet . M.delete (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by type (by fixed proxy)
deleteByTypeP' :: forall a c. (Typeable a, c a) => Proxy a -> HtsCSet c -> HtsCSet c
deleteByTypeP' _ = HtsCSet . M.delete (typeRep (Proxy :: Proxy a)) . unHS

-- | Delete an element by condition
deleteWhen :: forall a c. (Typeable a, c a) => (a -> Bool) -> HtsCSet c -> HtsCSet c
deleteWhen cond hs = case lookup hs of
    Nothing -> hs
    (Just a) -> if cond a then deleteByType a hs else hs


-- | Helper heterogeneous list for comfortable HtsSet building (with append and fill)
--
-- > let hs = fill ("a" :+ 'c' :+ True :+ ())
-- > lookup (hs :: HtsCSet Show) == Just 'c'
-- > use () to close the list
-- > lookup (hs :: HtsCSet Show) == Just () -- is False!
-- > let hs' = fill ("a" :+ 'c' :+ True :+ () :+ ())
-- > lookup (hs' :: HtsCSet Show) == Just () -- is Ok
data a :+ b = a :+ b
infixr 5 :+

class Append c a where
    append :: a -> HtsCSet c -> HtsCSet c

fill :: (Append c a) => a -> HtsCSet c
fill = flip append empty

instance (Typeable a, c a, Append c b) => Append c (a :+ b) where
    append (a :+ b) = insert a . (append b)

instance Append c () where
    append _ hs = hs
