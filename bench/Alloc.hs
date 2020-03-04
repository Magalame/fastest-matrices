{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where



import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector as V

-- DLA
import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Fast as MF
import qualified Statistics.Matrix.Fast.Algorithms as A

import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM


import qualified Statistics.Matrix.Function as M
import qualified Statistics.Matrix.Types as M
import Statistics.Matrix.Mutable  (unsafeNew,unsafeWrite,unsafeFreeze)

import Data.SIMD 
import qualified Data.Vector.Generic as VG
import GHC.Prim
import GHC.Types
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
               -- W.func "DLA - multiplication" (MF.multiply aDLA) bDLA
               -- W.func "DLA - multiplication2" (multiplyT3 aDLA) bDLA
               -- W.func "DLA - multiplication3" (multiplyT4 aDLA) bDLA
               -- W.func "DLA - multiplication4" (multiplyT4 aDLA) bDLA

               -- W.func "DLA - norm" MF.norm vDLA
               W.func "DLA - dot_simd4" (dot_simd4 vDLA) vDLA
               -- W.func "DLA - norm_simd" norm_simd vDLA

               -- W.func "Hmatrix - multiplication" ((<>) aH) bH
               W.func "Hmatrix - norm" H.norm_2 vH

               )

    -- print $ (MF.multiply aDLA) bDLA
    -- print $ (multiplyT4 aDLA) bDLA

multiplyT3 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT3 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = U.sum $ U.zipWith (*) (M.row m1 i) (M.row m2' j)
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 

multiplyT4 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT4 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = dot_simd4 (M.row m1 i) (M.row m2' j)
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 

unD# :: Double -> Double#
unD# (D# i#) = i#

multiplyT5 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT5 m1@(M.Matrix r1 c1 _) m2@(M.Matrix _ c2 _) = runST $ do
  let totLen = c1
      len = totLen `quot` 4
      rest =  4*len
        
  m3 <- unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = dot_simd_mul (M.row m1 i) (M.row m2' j) len rest 
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 

norm_simd :: U.Vector Double -> Double 
norm_simd v1 = sqrt $ dot_simd v1 v1

norm_simd4 :: U.Vector Double -> Double
norm_simd4 v = sqrt $ dot_simd4 v v

dot_simd4 :: U.Vector Double -> U.Vector Double -> Double 
dot_simd4 v1 v2 = plusHorizontal (go (broadcastDoubleX4# (int2Double# 0#) ) (len-1))
    where
        len = VG.length v1 `quot` 4

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)


dot_simd_mul :: U.Vector Double -> U.Vector Double -> Int -> Int -> Double 
dot_simd_mul !v1 !v2 !len !rest = remainder + plusHorizontal base
                 -- remainder + plusHorizontal base
    where
        -- (len,rest) = VG.length v1 `quotRem` 4
        
        zero# = int2Double# 0#

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)

        base = go (broadcastDoubleX4# zero#) (len-1)

        -- remainder | rest == 0 = (broadcastDoubleX4# zero#)

        --           | rest == 1 = let rem1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (len*4), zero#, zero#, zero# #)
        --                             rem2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (len*4), zero#, zero#, zero# #)
        --                         in timesDoubleX4# rem1 rem2

        --           | rest == 2 = let rem1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (len*4), unD# $ v1 `U.unsafeIndex` (len*4 + 1), zero#, zero# #)
        --                             rem2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (len*4), unD# $ v2 `U.unsafeIndex` (len*4 + 1), zero#, zero# #)
        --                         in timesDoubleX4# rem1 rem2

        --           | rest == 3 = let rem1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (len*4), unD# $ v1 `U.unsafeIndex` (len*4 + 1), unD# $ v1 `U.unsafeIndex` (len*4 + 2), zero# #)
        --                             rem2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (len*4), unD# $ v2 `U.unsafeIndex` (len*4 + 1), unD# $ v2 `U.unsafeIndex` (len*4 + 2), zero# #)
        --                         in timesDoubleX4# rem1 rem2

        remainder = U.sum $ U.zipWith (*) (U.drop rest v1) (U.drop rest v2)

        -- total = plusDoubleX4# base remainder

dot_simd :: U.Vector Double -> U.Vector Double -> Double 
dot_simd v1 v2 = remainder + plusHorizontal base
                 -- remainder + plusHorizontal base
    where
        -- (len,rest) = VG.length v1 `quotRem` 4
        totLen = VG.length v1
        len = totLen `quot` 4
        rest = 4*len
        
        zero# = int2Double# 0#

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)

        base = go (broadcastDoubleX4# zero#) (len-1)

        -- remainder | rest == 0 = (broadcastDoubleX4# zero#)

        --           | rest == 1 = let rem1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (len*4), zero#, zero#, zero# #)
        --                             rem2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (len*4), zero#, zero#, zero# #)
        --                         in timesDoubleX4# rem1 rem2

        --           | rest == 2 = let rem1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (len*4), unD# $ v1 `U.unsafeIndex` (len*4 + 1), zero#, zero# #)
        --                             rem2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (len*4), unD# $ v2 `U.unsafeIndex` (len*4 + 1), zero#, zero# #)
        --                         in timesDoubleX4# rem1 rem2

        --           | rest == 3 = let rem1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (len*4), unD# $ v1 `U.unsafeIndex` (len*4 + 1), unD# $ v1 `U.unsafeIndex` (len*4 + 2), zero# #)
        --                             rem2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (len*4), unD# $ v2 `U.unsafeIndex` (len*4 + 1), unD# $ v2 `U.unsafeIndex` (len*4 + 2), zero# #)
        --                         in timesDoubleX4# rem1 rem2

        remainder = U.sum $ U.zipWith (*) (U.drop rest v1) (U.drop rest v2)

        -- total = plusDoubleX4# base remainder

plusHorizontal :: DoubleX4# -> Double
plusHorizontal v = D# (r1+##r2+##r3+##r4)
  where
    !(# r1,r2,r3,r4 #) = unpackDoubleX4# v