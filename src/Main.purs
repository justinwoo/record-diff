module Main where

import Prelude

import Data.List (List)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand, inj, match)
import Effect (Effect)
import Effect.Console (log)
import Prim.Ordering (EQ, LT)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.Boolean (class If, class Or)
import Type.Data.Ordering as Ordering
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

class RowListIntersection
  (xs :: RL.RowList)
  (ys :: RL.RowList)
  (res :: RL.RowList)
  | xs ys -> res

instance rliNilNil :: RowListIntersection RL.Nil RL.Nil RL.Nil

instance rliNilXS :: RowListIntersection RL.Nil (RL.Cons name ty tail) RL.Nil

instance rliNilYS :: RowListIntersection (RL.Cons name ty tail) RL.Nil RL.Nil

instance rliConsCons ::
  ( Symbol.Compare xname yname ord
  , Ordering.Equals ord EQ isEq
  , Ordering.Equals ord LT isLt
  , Or isEq isLt isEqOrLt
  , If isEq xty trashty yty
  , If isEq xty trashty2 zty
  , If isEq (SProxy xname) trashname (SProxy zname)
  , If isEq
      (RLProxy (RL.Cons zname zty res'))
      (RLProxy res')
      (RLProxy res)
  , If isEqOrLt
      (RLProxy xs)
      (RLProxy (RL.Cons xname xty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (RL.Cons xname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res'
  ) => RowListIntersection (RL.Cons xname xty xs) (RL.Cons yname yty ys) res

rowListIntersection :: forall x xs y ys zs
   . RL.RowToList x xs
  => RL.RowToList y ys
  => RowListIntersection xs ys zs
  => { | x }
  -> { | y }
  -> RLProxy zs
rowListIntersection _ _ = RLProxy

testA :: RLProxy (RL.Cons "a" Int (RL.Cons "b" Int RL.Nil))
testA = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2 }

testB :: RLProxy (RL.Cons "a" Int (RL.Cons "b" Int RL.Nil))
testB = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2, c: "c" }

testC :: RLProxy (RL.Cons "a" Int (RL.Cons "b" Int RL.Nil))
testC = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2 }

testD :: RLProxy (RL.Cons "a" Int (RL.Cons "b" Int RL.Nil))
testD = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2, d: "d" }

class RecordDiff
  (rl :: RL.RowList) (r1 :: # Type) (r2 :: # Type) (tuples :: # Type)
  | rl -> r1 r2 tuples where
  recordDiff :: RLProxy rl -> { | r1 } -> { | r2 } -> List (Variant tuples)

instance rdNil :: RecordDiff RL.Nil trash1 trash2 () where
  recordDiff _ _ _ = mempty

instance rdCons ::
  ( IsSymbol name
  , Eq ty
  , Row.Cons name ty trash1 r1
  , Row.Cons name ty trash2 r2
  , Row.Cons name (Tuple ty ty) tuples' tuples
  , Row.Union tuples' trash tuples
  , RecordDiff tail r1 r2 tuples'
  ) => RecordDiff
         (RL.Cons name ty tail)
         r1 r2 tuples
    where
  recordDiff _ r1 r2 =
      first <> rest
    where
      namep = SProxy :: SProxy name
      first
        | l <- Record.get namep r1
        , r <- Record.get namep r2
        , l /= r = pure (inj namep (Tuple l r))
        | otherwise = mempty
      rest = expand <$> recordDiff (RLProxy :: RLProxy tail) r1 r2

mismatches :: forall r1 rl1 r2 rl2 rl tuples
   . RL.RowToList r1 rl1
  => RL.RowToList r2 rl2
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

main :: Effect Unit
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
