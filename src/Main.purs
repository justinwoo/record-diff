module Main where

import Prelude

import Data.List (List)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand, inj, match)
import Effect (Effect)
import Effect.Console (log)
import ExpectInferred (expectInferred)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.Boolean (class If)
import Type.Data.Ordering (class Equals)
import Type.Prelude (class IsSymbol, class RowToList, LT, Proxy(..), RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil, kind RowList)

class RowListIntersection
  (xs :: RowList)
  (ys :: RowList)
  (res :: RowList)
  | xs ys -> res

instance rliNilXS :: RowListIntersection Nil (Cons name ty tail) Nil
else instance rliNilYS :: RowListIntersection (Cons name ty tail) Nil Nil
else instance rliNilNil :: RowListIntersection Nil Nil Nil
else instance rliMatch ::
  ( RowListIntersection xTail yTail tail
  ) => RowListIntersection (Cons name ty xTail) (Cons name ty yTail) (Cons name ty tail)
else instance rliConsCons ::
  ( Symbol.Compare xname yname ord
  , Equals ord LT isLt
  , If isLt
      (RLProxy xs)
      (RLProxy (Cons xname xty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (Cons yname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res
  ) => RowListIntersection (Cons xname xty xs) (Cons yname yty ys) res

rowListIntersection :: forall x xs y ys zs
   . RowToList x xs
  => RowToList y ys
  => RowListIntersection xs ys zs
  => { | x }
  -> { | y }
  -> RLProxy zs
rowListIntersection _ _ = RLProxy

testA :: Unit
testA =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2 }
  in
    expectInferred expected actual

testB :: Unit
testB =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2, c: "c" }
  in
    expectInferred expected actual

testC :: Unit
testC =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2 }
  in
    expectInferred expected actual

testD :: Unit
testD =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2, d: "d" }
  in
    expectInferred expected actual

class RecordDiff
  (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (tuples :: # Type)
  | rl -> r1 r2 tuples where
  recordDiff :: RLProxy rl -> { | r1 } -> { | r2 } -> List (Variant tuples)

instance rdNil :: RecordDiff Nil trash1 trash2 () where
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
         (Cons name ty tail)
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
