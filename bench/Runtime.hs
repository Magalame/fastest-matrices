{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}

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

import qualified Criterion.Main as C

#define N 100
#define N2 10000

n :: Int
n = N

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

    vDLA' <- vectorGen
    uDLA' <- vectorGen

    let 

    --
      subDLA' = U.take n vDLA'
      aDLA' = M.Matrix n n vDLA'
      bDLA' = M.Matrix n n uDLA'
    
    --
      vList = U.toList vDLA'
      uList = U.toList uDLA'
    
    --
      aH' = (n H.>< n) vList
      bH' = (n H.>< n) uList

      subH' = H.fromList . take n $ vList
      vH' = H.fromList vList

    --
      aNH' = NP.fromList vList :: NH.Array V.Vector '[N, N] Double
      bNH' = NP.fromList uList :: NH.Array V.Vector '[N, N] Double

      vNH' = NP.fromList vList :: NH.Array V.Vector '[N2] Double

    --
      aDMX' = DMX.fromList n n vList
      bDMX' = DMX.fromList n n uList

    --
      vMA' = MA.fromList MA.Seq vList :: MA.Array MA.P MA.Ix1 Double
      aMA' = MA.resize' (MA.Sz (n MA.:. n)) vMA' :: MA.Array MA.P MA.Ix2 Double
      bMA' = MA.resize' (MA.Sz (n MA.:. n)) $ MA.fromList MA.Seq uList :: MA.Array MA.P MA.Ix2 Double

    C.defaultMain [ 
        C.env (pure (aDLA', bDLA', subDLA', vDLA')) $ \ ~(aDLA, bDLA, subDLA, vDLA) ->
            C.bgroup "DLA" [ 
                         C.bench "Matrix-matrix multiplication" $ C.nf (MF.multiply aDLA) bDLA,
                         C.bench "Matrix-matrix multiplication2" $ C.nf (multiplyT3 aDLA) bDLA,
                         C.bench "Matrix-matrix multiplication2" $ C.nf (multiplyT4 aDLA) bDLA,
                         C.bench "Matrix-matrix multiplication2" $ C.nf (multiplyT5 aDLA) bDLA
                         -- C.bench "Repeated matrix-matrix multiplication" $ C.nf (U.sum . (flip M.row) 1 . MF.multiply bDLA . MF.multiply aDLA . MF.multiply aDLA ) bDLA,
                         -- C.bench "Matrix-vector multiplication" $ C.nf (MF.multiplyV aDLA) subDLA,
                         -- C.bench "QR factorization" $ C.nf A.qr aDLA,
                         -- C.bench "Transpose" $ C.nf MF.transpose aDLA,
                         -- C.bench "Norm" $ C.nf MF.norm vDLA,
                         -- C.bench "Row" $ C.nf (M.row  aDLA) 0,
                         -- C.bench "Clumn" $ C.nf (M.column  aDLA) 0,
                         -- C.bench "Identity" $ C.nf M.ident n, 
                         -- C.bench "Diagonal" $ C.nf M.diag subDLA, 
                         -- C.bench "Map const 0" $ C.nf (M.map elemZero) aDLA,
                         -- C.bench "Map sqr" $ C.nf (M.map elemSqr) aDLA
                       ],

        C.env (pure (aH', bH', subH', vH')) $ \ ~(aH, bH, subH, vH) ->
            C.bgroup "Hmatrix" [ 
                             C.bench "Matrix-matrix multiplication" $ C.nf ((<>) aH) bH
                             -- C.bench "Repeated matrix-matrix multiplication" $ C.nf ( H.sumElements . flip (H.?) [1] . (<>) bH . (<>) aH . (<>) aH) bH,
                             -- C.bench "Matrix-vector multiplication" $ C.nf ((H.#>) aH) subH,
                             -- C.bench "QR factorization" $ C.nf H.qr aH,
                             -- C.bench "Transpose" $ C.nf H.tr aH,
                             -- C.bench "Norm" $ C.nf H.norm_2 vH,
                             -- C.bench "Row" $ C.nf ((H.?) aH) [0],
                             -- C.bench "Column" $ C.nf ((H.¿) aH) [0], 
                             -- C.bench "Identity" $ C.nf identH n,
                             -- C.bench "Diagonal" $ C.nf H.diag subH,
                             -- C.bench "Map const 0" $ C.nf (mapH elemZero) aH,
                             -- C.bench "Map sqr" $ C.nf (mapH elemSqr) aH
                           ]

        -- C.env (pure (aNH', bNH', vNH')) $ \ ~(aNH, bNH, vNH) ->
        --     C.bgroup "NumHask" [ 
        --                      C.bench "Matrix-matrix multiplication" $ C.nf (NH.mmult aNH) bNH,
        --                      C.bench "Repeated matrix-matrix multiplication" $ C.nf ( (\(NH.Array a) -> V.sum a) . NH.row (NP.Proxy :: NP.Proxy 1) . NH.mmult bNH . NH.mmult aNH . NH.mmult aNH ) bNH,
        --                      C.bench "Transpose" $ C.nf NH.transpose aNH,
        --                      C.bench "Norm" $ C.nf (sqrt . (NP.<.> vNH)) vNH,
        --                      C.bench "Row" $ C.nf (NH.row (NP.Proxy :: NP.Proxy 0)) aNH,
        --                      C.bench "Column" $ C.nf (NH.col (NP.Proxy :: NP.Proxy 0)) aNH
        --                    ],

        -- C.env (pure (aMA', bMA', vMA')) $ \ ~(aMA, bMA, vMA) ->
        --     C.bgroup "Massiv" [
        --                          C.bench "Matrix-matrix multiplication" $ C.nf ((MA.|*|) aMA) bMA,
        --                          C.bench "Matrix-matrix multiplication (Par)" $ C.nf ((MA.|*|) (MA.setComp MA.Par aMA)) bMA,
        --                          C.bench "Repeated matrix-matrix multiplication" $ C.nf ( MA.foldlS (+) 0 .  flip (MA.!>) 1 . (MA.|*|) bMA . (MA.|*|) aMA . (MA.|*|) aMA) bMA,
        --                          C.bench "Repeated matrix-matrix multiplication (Par)" $ C.nf ( MA.foldlS (+) 0 .  flip (MA.!>) 1 . (MA.|*|) bMA . (MA.|*|) aMA . (MA.|*|) (MA.setComp MA.Par aMA)) bMA,
        --                          C.bench "Transpose" $ C.nf (MA.computeAs MA.P . MA.transpose) aMA,
        --                          C.bench "Norm" $ C.nf (sqrt . MA.foldlS (+) 0 . (MA.zipWith (*) vMA)) vMA,
        --                          C.bench "Row" $ C.nf (MA.computeAs MA.P . (MA.!>) aMA) 0,
        --                          C.bench "Column" $ C.nf (MA.computeAs MA.P . (MA.<!) aMA) 0
        --                          ],

        -- C.env (pure (aDMX', bDMX')) $ \ ~(aDMX, bDMX) ->
        --         C.bgroup "Matrix" [ 
        --                          C.bench "Matrix-matrix multiplication" $ C.nf (DMX.multStrassenMixed aDMX) bDMX,
        --                          C.bench "Transpose" $ C.nf DMX.transpose aDMX,
        --                          C.bench "Row" $ C.nf (DMX.getRow 1) aDMX,
        --                          C.bench "Column" $ C.nf (DMX.getCol 1) aDMX,
        --                          C.bench "Identity" $ C.nf identDMX n
        --                          ]
                  ]


    -- print $ (MF.multiply aDLA') bDLA'
    -- print $ (multiplyT3 aDLA') bDLA'


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
        z = norm_simd4 (M.row m1 i) (M.row m2' j)
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 


norm_simd4 :: U.Vector Double -> U.Vector Double -> Double 
norm_simd4 v1 v2 = plusHorizontalX4 $ go 0 (VG.length v1'-1)
    where
        v1' = unsafeVectorizeUnboxedX4 v1
        v2' = unsafeVectorizeUnboxedX4 v2

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+mul
                mul = c1*c2
                c1 = v1' `VG.unsafeIndex` i
                c2 = v2' `VG.unsafeIndex` i



multiplyT5 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT5 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = norm_simd5 (M.row m1 i) (M.row m2' j)
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 

unD# :: Double -> Double#
unD# (D# i#) = i#


-- norm_simd4 :: U.Vector Double -> U.Vector Double -> Double 
-- norm_simd4 v1 v2 = plusHorizontal $ go (broadcastDoubleX4# (int2Double# 0#) ) (len-1)
--     where
--         len = VG.length v1 `quot` 4

--         go tot (-1) = tot
--         go tot i = go tot' (i-1)
--             where
--                 tot' = plusDoubleX4# tot mul
--                 mul = timesDoubleX4# c1 c2
--                 c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
--                 c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)

norm_simd5 :: U.Vector Double -> U.Vector Double -> Double 
norm_simd5 v1 v2 = plusHorizontal (go (broadcastDoubleX4# (int2Double# 0#) ) (len-1))
    where
        len = VG.length v1 `quot` 4

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)


plusHorizontal :: DoubleX4# -> Double
plusHorizontal v = D# (r1+##r2+##r3+##r4)
  where
    (# r1,r2,r3,r4 #) = unpackDoubleX4# v