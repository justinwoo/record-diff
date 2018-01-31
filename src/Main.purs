module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List)
import Data.Monoid (mempty)
import Data.Record (get)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand, inj, match)
import Type.Data.Boolean (class If, class Or)
import Type.Data.Ordering (class Equals)
import Type.Prelude (class CompareSymbol, class IsSymbol, class RowToList, EQ, LT, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil, kind RowList)

class RowListIntersection
  (xs :: RowList)
  (ys :: RowList)
  (res :: RowList)
  | xs ys -> res

instance rliNilXS :: RowListIntersection Nil (Cons name ty tail) Nil
instance rliNilYS :: RowListIntersection (Cons name ty tail) Nil Nil
instance rliNilNil :: RowListIntersection Nil Nil Nil
instance rliConsCons ::
  ( CompareSymbol xname yname ord
  , Equals ord EQ isEq
  , Equals ord LT isLt
  , Or isEq isLt isEqOrLt
  , If isEq xty trashty yty
  , If isEq xty trashty2 zty
  , If isEq (SProxy xname) trashname (SProxy zname)
  , If isEq
      (RLProxy (Cons zname zty res'))
      (RLProxy res')
      (RLProxy res)
  , If isEqOrLt
      (RLProxy xs)
      (RLProxy (Cons xname xty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (Cons xname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res'
  ) => RowListIntersection (Cons xname xty xs) (Cons yname yty ys) res

rowListIntersection :: forall x xs y ys zs
   . RowToList x xs
  => RowToList y ys
  => RowListIntersection xs ys zs
  => { | x }
  -> { | y }
  -> RLProxy zs
rowListIntersection _ _ = RLProxy

testA :: RLProxy (Cons "a" Int (Cons "b" Int Nil))
testA = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2 }

testB :: RLProxy (Cons "a" Int (Cons "b" Int Nil))
testB = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2, c: "c" }

testC :: RLProxy (Cons "a" Int (Cons "b" Int Nil))
testC = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2 }

testD :: RLProxy (Cons "a" Int (Cons "b" Int Nil))
testD = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2, d: "d" }

class RecordDiff
  (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (tuples :: # Type)
  | rl -> r1 r2 tuples where
  recordDiff :: RLProxy rl -> { | r1 } -> { | r2 } -> List (Variant tuples)

instance rdNil :: RecordDiff Nil trash1 trash2 () where
  recordDiff _ _ _ = mempty

instance rdCons ::
  ( IsSymbol name
  , Eq ty
  , RowCons name ty trash1 r1
  , RowCons name ty trash2 r2
  , RowCons name (Tuple ty ty) tuples' tuples
  , Union tuples' trash tuples
  , RecordDiff tail r1 r2 tuples'
  ) => RecordDiff
         (Cons name ty tail)
         r1 r2 tuples
    where
  recordDiff _ r1 r2 =
      first <> rest
    where
      namep = SProxy :: SProxy name
      first
        | l <- get namep r1
        , r <- get namep r2
        , l /= r = pure (inj namep (Tuple l r))
        | otherwise = mempty
      rest = expand <$> recordDiff (RLProxy :: RLProxy tail) r1 r2

mismatches :: forall r1 rl1 r2 rl2 rl tuples
   . RowToList r1 rl1
  => RowToList r2 rl2
  => RowListIntersection rl1 rl2 rl
  => RecordDiff rl r1 r2 tuples
  => { | r1 }
  -> { | r2 }
  -> List (Variant tuples)
mismatches r1 r2 = recordDiff (RLProxy :: RLProxy rl) r1 r2

test1 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test1 = mismatches { a: 1, b: 2 } { a: 2, b: 2 }

test2 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test2 = mismatches { a: 1, b: 2 } { a: 2, b: 2, c: "c" }

test3 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test3 = mismatches { a: 1, b: 2, c: "c" } { a: 1, b: 3 }

test4 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test4 = mismatches { a: 1, b: 2, c: "c" } { a: 2, b: 2, d: "d" }

main :: forall e. Eff ( console :: CONSOLE | e ) Unit
main = do
  traverse_ log' test1
  traverse_ log' test2
  traverse_ log' test3
  traverse_ log' test4
  -- output:
  -- a was different: (Tuple 1 2)
  -- a was different: (Tuple 1 2)
  -- b was different: (Tuple 2 3)
  -- a was different: (Tuple 1 2)
    where
      log' = match
        { a: \x -> log $ "a was different: " <> show x
        , b: \x -> log $ "b was different: " <> show x
        }
