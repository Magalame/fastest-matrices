{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}


module Main where



import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector as V

-- DLA
import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Fast as MF
import qualified Statistics.Matrix.Fast.Algorithms as A

-- hmatrix
import qualified Numeric.LinearAlgebra as H

-- numhask
import qualified NumHask.Array as NH
import qualified NumHask.Prelude as NP

-- data.matrix from matrix
import qualified Data.Matrix as DMX

-- massiv
import qualified Data.Massiv.Array as MA

import qualified System.Random.MWC as Mwc

import qualified Weigh as W

n :: Int
n = 100

vectorGen :: IO (Vector Double)
vectorGen =  do 
    gen <- Mwc.create
    Mwc.uniformVector gen (n*n)

matrixDLA :: IO M.Matrix
matrixDLA = do
    vec <- vectorGen
    return $ M.Matrix n n vec

matrixH :: IO (H.Matrix H.R)
matrixH = do
    vec <- vectorGen
    return $ (n H.>< n) $ U.toList $ vec 

identH :: Int -> H.Matrix Double
identH = H.ident

identDMX :: Int -> DMX.Matrix Double
identDMX = DMX.identity


elemZero :: Double -> Double
elemZero = const 0

elemSqr :: Double -> Double
elemSqr x = x*x

mapH :: (Double -> Double) -> H.Matrix Double -> H.Matrix Double
mapH = H.cmap

main :: IO ()
main = do 

    let 

    vDLA <- vectorGen
    uDLA <- vectorGen

    let 

    --
      subDLA = U.take n vDLA
      aDLA = M.Matrix n n vDLA
      bDLA = M.Matrix n n uDLA
    
    --
      vList = U.toList vDLA
      uList = U.toList uDLA
    
    --
      aH = (n H.>< n) vList
      bH = (n H.>< n) uList

      subH = H.fromList . take n $ vList
      vH = H.fromList vList

    --
      aNH = NP.fromList vList :: NH.Array V.Vector '[50, 50] Double
      bNH = NP.fromList uList :: NH.Array V.Vector '[50, 50] Double

      subNH = NP.fromList . take n $ vList :: NH.Array V.Vector '[50] Double
      vNH = NP.fromList vList :: NH.Array V.Vector '[2500] Double

    --
      aDMX = DMX.fromList n n vList
      bDMX = DMX.fromList n n uList

    -- 
      vMA = MA.fromList MA.Seq vList :: MA.Array MA.P MA.Ix1 Double
      aMA = MA.resize' (MA.Sz (n MA.:. n)) vMA :: MA.Array MA.P MA.Ix2 Double
      bMA = MA.resize' (MA.Sz (n MA.:. n)) $ MA.fromList MA.Seq uList :: MA.Array MA.P MA.Ix2 Double


    W.mainWith (do 
               W.func "sum_vec"           sum_vec aDLA
               W.func "sum_vec mul"      (sum_vec . MF.multiply bDLA) bDLA
               W.func "sum_vec mul fuse" (sum_vec . multiplyFuse bDLA) bDLA
               W.func "sum_vec mul row"  (sum_vec_row . multiplyFuse bDLA) bDLA
               W.func "sum_vec fused "   (multiplyFused bDLA) bDLA
               W.func "sum_vec fused2 "  (multiplyFused2 bDLA) bDLA


               )

    print $ (sum_vec_row . multiplyFuse bDLA) bDLA
    print $ (multiplyFused bDLA) bDLA
    print $ (multiplyFused2 bDLA) bDLA

    print $ (multiplyFused2 bDLA) bDLA

row :: M.Matrix -> Int -> Vector Double
row m i = U.slice (c*i) c v
    where c = M.cols m
          v = M._vector m
{-# INLINE [1] row #-}

column :: M.Matrix -> Int -> Vector Double
column m j= U.generate r (\i -> v `U.unsafeIndex` (j + i * c))
        where r = M.rows m
              c = M.cols m
              v = M._vector m
{-# INLINE column #-}

column2 :: M.Matrix -> Int -> Vector Double
column2 m j = U.generate (M.rows m) (\i -> (M._vector m) `U.unsafeIndex` (j + i * (M.cols m)))
{-# INLINE column2 #-}

sum_vec_row :: M.Matrix -> Double
sum_vec_row a = (U.sum . flip row 0) a
{-# INLINE sum_vec_row #-}

sum_vec :: M.Matrix -> Double
sum_vec a = (U.sum . flip column2 0) a
{-# INLINE sum_vec #-}

sum_vec2 :: M.Matrix -> Double
sum_vec2 a = (U.sum . flip column 0) a
{-# INLINE sum_vec2 #-}

multiplyFuse :: M.Matrix -> M.Matrix -> M.Matrix
multiplyFuse m1 m2 = M.Matrix r1 c2 $ U.generate (r1*c2) go
  where
    r1 = M.rows m1
    c2 = M.cols m2
    go t = U.sum $ U.zipWith (*) (row m1 i) (M.column m2 j)
      where (i,j) = t `quotRem` c2
{-# INLINE multiplyFuse #-}

multiplyFused m1 m2 = U.sum $ U.slice 0 n (U.generate (r1*c2) go)
  where
    r1 = M.rows m1
    c2 = M.cols m2
    go t = U.sum $ U.zipWith (*) (row m1 i) (M.column m2 j)
      where (i,j) = t `quotRem` c2

-- multiplyFused m1 m2 = U.sum $ U.slice 0 n $ U.generate (r1*c2) go
--   where
--     r1 = M.rows m1
--     c2 = M.cols m2
--     go t = U.sum $ U.zipWith (*) (M.row m1 i) (M.column m2 j)
--       where (i,j) = t `quotRem` c2

      

multiplyFused2 m1 m2 = U.sum $ row (M.Matrix n n $ U.generate (r1*c2) go) 0
  where
    r1 = M.rows m1
    c2 = M.cols m2
    go t = U.sum $ U.zipWith (*) (row m1 i) (M.column m2 j)
      where (i,j) = t `quotRem` c2

{-# RULES
      "row/fuse"    forall c v r i.  row (M.Matrix r c v) i = U.slice (c*i) c v
  #-}

