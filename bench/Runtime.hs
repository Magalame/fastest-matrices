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



import qualified Criterion.Main as C

import Debug.Trace
import Control.DeepSeq


import Common

main :: IO ()
main = do 

    C.defaultMain [ 
        C.env (pure (aDLA', bDLA', subDLA', vDLA')) $ \ ~(aDLA, bDLA, subDLA, vDLA) ->
            C.bgroup "DLA" [ 
                         -- C.bench "Matrix-matrix multiplication" $ C.nf (MF.multiply aDLA) bDLA,
                         -- C.bench "Matrix-matrix multiplication w transpose" $ C.nf (multiplyT3 aDLA) bDLA,
                         -- C.bench "Matrix-matrix multiplication simd 4" $ C.nf (multiplyT4 aDLA) bDLA
                          C.bench "Matrix-matrix multiplication simd opti" $ C.nf (multiplyT5 aDLA) bDLA
                          ,C.bench "Matrix-matrix multiplication simd" $ C.nf (multiplyT6 aDLA) bDLA
                         -- C.bench "Repeated matrix-matrix multiplication" $ C.nf (U.sum . (flip M.row) 1 . MF.multiply bDLA . MF.multiply aDLA . MF.multiply aDLA ) bDLA,
                         -- C.bench "Matrix-vector multiplication" $ C.nf (MF.multiplyV aDLA) subDLA,
                         -- C.bench "QR factorization" $ C.nf A.qr aDLA,
                         -- ,C.bench "Transpose" $ C.nf MF.transpose aDLA
                         -- C.bench "Transpose 8" $ C.nf transpose8 aDLA,
                         -- C.bench "Transpose 8'" $ C.nf transpose8' aDLA
                         -- ,C.bench "Norm2" $ C.nf (dot_simd vDLA) vDLA
                         -- ,C.bench "Norm3" $ C.nf (dot_simd_mul vDLA vDLA 2500) 0
                         ,C.bench "Norm" $ C.nf MF.norm vDLA
                         ,C.bench "Norm simd 4" $ C.nf norm_simd_bis vDLA
                         ,C.bench "Norm simd full" $ C.nf norm_simd vDLA
                         -- ,C.bench "Norm simd" $ C.nf (dot_simd vDLA) vDLA
                         -- C.bench "Norm simd 4" $ C.nf (dot_simd_bis vDLA) vDLA
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
                             -- C.bench "Dot" $ C.nf (vH H.<.>) vH
                             ,C.bench "Norm" $ C.nf H.norm_2 vH
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
    -- print $ (multiplyT5  aDLA') bDLA'
    print $ norm_simd_bis vDLA'
    print $ norm_simd vDLA' 
    print $ MF.norm vDLA'
    print $ H.norm_2 vH'
    -- print $ transpose8' aDLA' == M.transpose aDLA'



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

-- multiplyT4 :: M.Matrix -> M.Matrix -> M.Matrix
-- multiplyT4 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
--   m3 <- unsafeNew r1 c2
--   M.for 0 c2 $ \j -> do
--     M.for 0 r1 $ \i -> do
--       let 
--         z = dot_simd_cp (M.row m1 i) (M.row m2' j)
--       unsafeWrite m3 i j z
--   unsafeFreeze m3
--     where m2' = MF.transpose m2 


-- dot_simd_cp :: U.Vector Double -> U.Vector Double -> Double 
-- dot_simd_cp v1 v2 = plusHorizontalX4 $ go 0 (VG.length v1'-1)
--     where
--         v1' = unsafeVectorizeUnboxedX4 v1
--         v2' = unsafeVectorizeUnboxedX4 v2

--         go tot (-1) = tot
--         go tot i = go tot' (i-1)
--             where
--                 tot' = tot+mul
--                 mul = c1*c2
--                 c1 = v1' `VG.unsafeIndex` i
--                 c2 = v2' `VG.unsafeIndex` i

multiplyT4 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT4 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = dot_simd_bis (M.row m1 i) (M.row m2' j)
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 

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


multiplyT6 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT6 m1@(M.Matrix r1 c1 _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = dot_simd (M.row m1 i) (M.row m2' j)
      unsafeWrite m3 i j z
  unsafeFreeze m3
    where m2' = MF.transpose m2 


unD# :: Double -> Double#
unD# (D# i#) = i#



-- dot_simd_cp :: U.Vector Double -> U.Vector Double -> Double 
-- dot_simd_cp v1 v2 = plusHorizontal $ go (broadcastDoubleX4# (int2Double# 0#) ) (len-1)
--     where
--         len = VG.length v1 `quot` 4

--         go tot (-1) = tot
--         go tot i = go tot' (i-1)
--             where
--                 tot' = plusDoubleX4# tot mul
--                 mul = timesDoubleX4# c1 c2
--                 c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
--                 c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)

norm_simd_bis :: U.Vector Double -> Double
norm_simd_bis v = sqrt $ dot_simd_bis v v 

dot_simd_bis :: U.Vector Double -> U.Vector Double -> Double 
dot_simd_bis v1 v2 = plusHorizontal (go (broadcastDoubleX4# (int2Double# 0#) ) (len-1))
    where
        len = VG.length v1 `quot` 4

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)


dot_simd :: U.Vector Double -> U.Vector Double -> Double 
dot_simd v1 v2 = remainder + plusHorizontal base
                 -- remainder + plusHorizontal base
    where
        -- (len,rest) = VG.length v1 `quotRem` 4
        totLen = VG.length v1
        len = totLen `quot` 4
        rest = 4*len
        
        zero_local# = int2Double# 0#

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)

        base = go (broadcastDoubleX4# zero_local#) (len-1)

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

dot_simd_mul :: U.Vector Double -> U.Vector Double -> Int -> Int -> Double 
dot_simd_mul !v1 !v2 !len !rest = remainder + plusHorizontal base
                 -- remainder + plusHorizontal base
    where
        -- (len,rest) = VG.length v1 `quotRem` 4
        
        zeroVec# = broadcastDoubleX4# (int2Double# 0#)

        go !tot (-1) = tot
        go !tot !i = go tot' (i-1)
            where
                tot' = plusDoubleX4# tot mul
                mul = timesDoubleX4# c1 c2
                c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)
                c2 = packDoubleX4# (# unD# $ v2 `U.unsafeIndex` (i*4 + 0), unD# $ v2 `U.unsafeIndex` (i*4 + 1), unD# $ v2 `U.unsafeIndex` (i*4 + 2), unD# $ v2 `U.unsafeIndex` (i*4 + 3) #)

        base = go zeroVec# (len-1)

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

norm_simd :: U.Vector Double -> Double 
norm_simd v1 = sqrt $ dot_simd v1 v1

-- norm_simd :: U.Vector Double -> Double 
-- norm_simd v1 = sqrt $ plusHorizontal (go (broadcastDoubleX4# (int2Double# 0#) ) (len-1))
--     where
--         len = VG.length v1 `quot` 4

--         go tot (-1) = tot
--         go tot i = go tot' (i-1)
--             where
--                 tot' = plusDoubleX4# tot mul
--                 mul = timesDoubleX4# c1 c1
--                 c1 = packDoubleX4# (# unD# $ v1 `U.unsafeIndex` (i*4 + 0), unD# $ v1 `U.unsafeIndex` (i*4 + 1), unD# $ v1 `U.unsafeIndex` (i*4 + 2), unD# $ v1 `U.unsafeIndex` (i*4 + 3) #)

-- | Simple for loop.  Counts from /start/ to /end/-1 by +m.
form :: Monad m => Int -> (Int -> Bool) -> Int -> (Int -> m ()) -> m ()
form n0 check gap f = loop n0
  where
    loop i | check i   = f i >> loop (i+gap)
           | otherwise = return ()
{-# INLINE form #-}

transpose8 :: M.Matrix -> M.Matrix
transpose8 (M.Matrix r0 c0 v0) 
  = M.Matrix c0 r0 $ runST $ do
    vec <- UM.unsafeNew (r0*c0)

    form 0 (\i -> i < r0) block $ \i -> do
 
      M.for 0 c0 $ \j -> do

        form 0 (\b -> b < block && i+b < r0) 1 $ \b -> do

          let tmp = v0 `U.unsafeIndex` ((i + b)*c0 + j)

          UM.unsafeWrite vec (j*r0 + i + b) tmp

          -- traceM $ (++) "index:" $ show (j,i+b)

    U.unsafeFreeze vec
      where block = 2

transpose8' :: M.Matrix -> M.Matrix
transpose8' (M.Matrix r0 c0 v0) 
  = M.Matrix c0 r0 $ runST $ do
    vec <- UM.unsafeNew (r0*c0)

    form 0 (\i -> i < r0) block $ \i -> do
 
      M.for 0 c0 $ \j -> do

        form 0 (\b -> b < block && i+b < r0) 1 $ \b -> do

          let tmp = v0 `U.unsafeIndex` (j*r0 + i + b)

          UM.unsafeWrite vec  ((i + b)*c0 + j) tmp

          traceM $ (++) "index:" $ show ((i + b),j)

    U.unsafeFreeze vec
      where block = 2



-- void transpose(double *dst, const double *src, size_t n, size_t p) noexcept {
--     THROWS();
--     size_t block = 32;
--     for (size_t i = 0; i < n; i += block) {
--         for(size_t j = 0; j < p; ++j) {
--             for(size_t b = 0; b < block && i + b < n; ++b) {
--                 dst[j*n + i + b] = src[(i + b)*p + j];
--             }
--         }
--     }
-- }