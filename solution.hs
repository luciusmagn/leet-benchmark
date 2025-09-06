{-# LANGUAGE Haskell2010
  , KindSignatures
  , NumericUnderscores
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where


-- + Imports

-- ++ from base ^>= 4.21

import Data.Kind
  ( Type )

import GHC.Exts
  ( lazy )

import Control.Monad.ST
  ( runST )

import Data.Foldable
  ( traverse_ )

import System.CPUTime
  ( getCPUTime )

-- ++ from primitive ^>= 0.9.1

import Control.Monad.Primitive
  ( PrimMonad
  , PrimState
  )

import Data.Primitive.Types
  ( Prim )

import Data.Primitive.PrimArray
  ( MutablePrimArray
  , writePrimArray
  , readPrimArray
  , unsafeThawPrimArray
  , unsafeFreezePrimArray
  , replicatePrimArray
  , primArrayToList
  )


-- * Solution

{- | Modify a 'Data.Primitive.PrimArray' at a given index\;
    suprising that this isn't already in "Data.Primitive.PrimArray"...
-}
{-# INLINE modifyPrimArray #-}
modifyPrimArray :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    MutablePrimArray (PrimState m) a -> Int -> (a -> a) -> m ()
modifyPrimArray = \ wa i f -> do
    a <- readPrimArray wa i
    writePrimArray wa i $ f a

{- | The index of a lower case English letter
    (starting from @alphPosLwr \'a\' = 0@)
-}
{-# INLINE alphPosLwr #-}
alphPosLwr :: Char -> Int
alphPosLwr = subtract (fromEnum 'a') . fromEnum

{- | Solution\; apparently hurts to INLINE! #-}
{-# INLINEABLE solution #-}
solution :: String -> String -> Int
solution = \ sx sy -> runST $ do
    wc <- unsafeThawPrimArray $ replicatePrimArray 26 0
    traverse_ (\ c -> modifyPrimArray wc (alphPosLwr c) (+        1)) sx
    traverse_ (\ c -> modifyPrimArray wc (alphPosLwr c) (subtract 1)) sy
    sum . filter (> 0) . primArrayToList <$> unsafeFreezePrimArray wc


-- * Benchmark

{- | Forces in turn the elements of the first argument
    and then evaluates to its second, necessarily in that sequence
    (via the same idea as 'Control.Parallel.pseq')
-}
{-# INLINE pseqElems #-}
pseqElems :: forall a b. [a] -> b -> b
pseqElems = flip $ foldr (\ a b -> a `seq` lazy b)

{- | Our basic test cases\; NOINLINEd so that
    GHC can\'t statically perform any of
    the work meant to occur at runtime!
-}
{-# NOINLINE testCases #-}
testCases :: [(String, String)]
testCases =
  [ ("bab", "aba")
  , ("leetcode", "practice")
  , ("anagram", "mangaar")
  , (replicate 50000 'a', replicate 50000 'b')
  ]

{- | Shows a time value in picoseconds as
    one in miliseconds to the second decimal place
-}
showPicos :: Integer -> String
showPicos = \ n -> case flip quotRem 100 $ flip quot 10_000_000 n of
    (q, r) -> show q <> "." <> show r <> "ms"

{- | main -}
main :: IO ()
main = do
    let testCount = length testCases
        test = uncurry solution . (testCases !!) . (`rem` testCount)
    -- Warmup prior to measuring start time
    start <- fmap test [0..100 * testCount - 1] `pseqElems` getCPUTime
    -- Internal benchmark prior to measuring end time
    end <- fmap test [0..10000 * testCount - 1] `pseqElems` getCPUTime
    putStrLn . ("Haskell internal benchmark: " <>) . showPicos $ end - start
