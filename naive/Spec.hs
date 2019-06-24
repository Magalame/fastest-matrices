{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Inspection

import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)

import System.IO.Unsafe

import Data.List (foldl')


-- mulSub :: Vector Double -> Double
-- mulSub v1 =  U.foldl' (\acc x -> acc + x*x) 0 v1

-- inspect $ ('mulSub `hasNoType` ''Vector) 

sumUp1 :: Int -> Int
sumUp1 n = sum [1..n]

inspect $ 'sumUp1 `hasNoType` ''[]

sumUp2 :: Int -> Int
sumUp2 n = foldl' (\acc x -> acc + x*x) 0 [1..n]

inspect $ 'sumUp2 `hasNoType` ''[]

v :: Vector Double
v = U.fromList [1,2,3]

u :: Vector Double
u = U.fromList [1,2,3]

mulSub :: Vector Double -> Vector Double -> Double
mulSub v1 v2 =  U.sum $ U.zipWith (*) v1 v2

mul :: Double 
mul = mulSub v u

main :: IO ()
main = print "tests passed"