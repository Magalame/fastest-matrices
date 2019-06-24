{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}


module Main where



import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector as V

import qualified Data.Vector.Unboxed.Mutable as UM

-- DLA
import qualified Statistics.Matrix as M
import qualified Statistics.Matrix.Mutable as M
import qualified Statistics.Matrix.Function as M
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

import qualified Criterion.Main as C

import Debug.Trace
import Control.Monad.ST

import Control.DeepSeq

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

normMA :: MA.Array MA.U MA.Ix1 Double -> Double
normMA v = sqrt $ MA.foldlS (+) 0 $ (MA.zipWith (*) v v)

transposeMA :: MA.Array MA.U MA.Ix2 Double -> MA.Array MA.U MA.Ix2 Double
transposeMA = MA.computeAs MA.U . MA.transpose

-- transpose :: M.Matrix -> M.Matrix
-- transpose m@(M.Matrix r0 c0 v0) 
--   = M.Matrix c0 r0 $ runST $ do
--     vec <- UM.unsafeNew (r0*c0)
--     M.for 0 r0 $ \i -> do
--       UM.unsafeWrite vec (i + i * c0) $ v0 `U.unsafeIndex` (i + i * c0)
--       M.for (i+1) c0 $ \j -> do
--         let tmp = v0 `U.unsafeIndex` (j + i * c0)
--             tmp2 = v0 `U.unsafeIndex` (i + j * c0)
--         UM.unsafeWrite vec (j + i * c0) tmp2
--         UM.unsafeWrite vec (i + j * c0) tmp
--     U.unsafeFreeze vec

-- transpose :: M.Matrix -> M.Matrix
-- transpose m@(M.Matrix r0 c0 v0) 
--   = M.Matrix c0 r0 $ runST $ do
--     vec <- UM.unsafeNew (r0*c0)
--     M.for 0 r0 $ \i -> do
--       UM.unsafeWrite vec (i + i * c0) $ v0 `U.unsafeIndex` (i + i * c0)
--       M.for (i+1) c0 $ \j -> do
--         let tmp = v0 `U.unsafeIndex` (j + i * c0)
--             tmp2 = v0 `U.unsafeIndex` (i + j * c0)
--         UM.unsafeWrite vec (j + i * c0) tmp2
--         UM.unsafeWrite vec (i + j * c0) tmp
--     U.unsafeFreeze vec

main :: IO ()
main = do

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
      aNH = NP.fromList vList :: NH.Array V.Vector '[100, 100] Double
      bNH = NP.fromList uList :: NH.Array V.Vector '[100, 100] Double

      subNH = NP.fromList . take n $ vList :: NH.Array V.Vector '[100] Double
      vNH = NP.fromList vList :: NH.Array V.Vector '[10000] Double

    --
      aDMX = DMX.fromList n n vList
      bDMX = DMX.fromList n n uList

    --
      vMA = MA.fromList MA.Seq vList :: MA.Array MA.U MA.Ix1 Double
      aMA = MA.resize' (MA.Sz $ MA.Ix2 n n) vMA :: MA.Array MA.U MA.Ix2 Double
      bMA = MA.resize' (MA.Sz $ MA.Ix2 n n) $ MA.fromList MA.Seq uList :: MA.Array MA.U MA.Ix2 Double

    deepseq subDLA $ deepseq aDLA $ deepseq bDLA $ return ()
    deepseq vMA $ deepseq aMA $ deepseq bMA $ return ()

    C.defaultMain [ 
        C.bgroup "DLA" [ 
                         C.bench "multiplication" $ C.nf (MF.multiply aDLA) bDLA,
                         C.bench "mulT" $ C.nf  (multiplyT aDLA) aDLA,
                         -- C.bench "mulT2" $ C.nf (multiplyT2 aDLA) aDLA
                         C.bench "mulT3" $ C.nf (multiplyT3 aDLA) aDLA
                         -- C.bench "mulT4" $ C.nf (multiplyT4 aDLA) aDLA
                         -- C.bench "repeated multiplication" $ C.nf (U.sum . (flip M.row) 1 . MF.multiply bDLA . MF.multiply aDLA . MF.multiply aDLA ) bDLA,
                         -- C.bench "multiplicationV" $ C.nf (MF.multiplyV aDLA) subDLA,

                         -- C.bench "qr factorization" $ C.nf A.qr aDLA,

                         -- C.bench "transpose" $ C.nf MF.transpose aDLA

                         -- C.bench "norm" $ C.nf MF.norm vDLA,

                         -- C.bench "norm2" $ C.nf MF.norm2 vDLA
                         -- C.bench "row" $ C.nf (M.row  aDLA) 0,
                         -- C.bench "column" $ C.nf (M.column  aDLA) 0,

                         -- C.bench "identity" $ C.nf M.ident n,
                         -- C.bench "diag" $ C.nf M.diag subDLA, 
                         -- C.bench "map const 0" $ C.nf (M.map elemZero) aDLA,
                         -- C.bench "map sqr" $ C.nf (M.map elemSqr) aDLA
                       ],
        C.bgroup "Hmatrix" [ 
                             C.bench "multiplication" $ C.nf ((<>) aH) bH
                             -- C.bench "repeated multiplication" $ C.nf ( H.sumElements . flip (H.?) [1] . (<>) bH . (<>) aH . (<>) aH) bH,
                             -- C.bench "multiplicationV" $ C.nf ((H.#>) aH) subH,

                             -- C.bench "qr factorization" $ C.nf H.qr aH,

                             -- C.bench "transpose" $ C.nf H.tr aH

                             -- C.bench "norm" $ C.nf H.norm_2 vH,
                             -- C.bench "row" $ C.nf ((H.?) aH) [0],
                             -- C.bench "column" $ C.nf ((H.¿) aH) [0], 

                             -- C.bench "identity" $ C.nf identH n,
                             -- C.bench "diag" $ C.nf H.diag subH,
                             -- C.bench "map const 0" $ C.nf (mapH elemZero) aH,
                             -- C.bench "map sqr" $ C.nf (mapH elemSqr) aH
                           ],
        -- C.bgroup "NumHask" [ 
        --                      C.bench "multiplication" $ C.nf (NH.mmult aNH) bNH,
        --                      C.bench "repeated multiplication" $ C.nf ( (\(NH.Array a) -> V.sum a) . NH.row (NP.Proxy :: NP.Proxy 1) . NH.mmult bNH . NH.mmult aNH . NH.mmult aNH ) bNH,

        --                      C.bench "transpose" $ C.nf NH.transpose aNH,

        --                      C.bench "norm" $ C.nf (sqrt . (NP.<.> vNH)) vNH,

        --                      C.bench "row" $ C.nf (NH.row (NP.Proxy :: NP.Proxy 0)) aNH,
        --                      C.bench "column" $ C.nf (NH.col (NP.Proxy :: NP.Proxy 0)) aNH
        --                    ],
        C.bgroup "Massiv" [                                  
                                 C.bench "multiplication" $ C.nf ((MA.|*|) aMA) bMA
                                 -- C.bench "transpose" $ C.nf transposeMA aMA
                                 -- C.bench "row" $ C.nf (MA.computeAs MA.U . (MA.!>) aMA) 0
                                 ]
        -- C.bgroup "Matrix" [ 
        --                          C.bench "multiplication" $ C.nf (DMX.multStrassenMixed aDMX) bDMX,
        --                          C.bench "transpose" $ C.nf DMX.transpose aDMX,
        --                          C.bench "row" $ C.nf (DMX.getRow 1) aDMX,
        --                          C.bench "column" $ C.nf (DMX.getCol 1) aDMX,
        --                          C.bench "identity" $ C.nf identDMX n
        --                          ]
                  ]
    
    print $ M.transpose aDLA == MF.transpose aDLA
    print $ (MF.multiply aDLA) bDLA == (multiplyT aDLA) aDLA 
    print $ (multiplyT3 aDLA) aDLA == (MF.multiply aDLA) bDLA

multiplyT :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) =
  M.Matrix r1 c2 $ U.generate (r1*c2) go
  where
    go t = U.sum $ U.zipWith (*) (M.row m1 i) (M.row m2' j)
      where (i,j) = t `quotRem` c2
            m2' = MF.transpose m2

multiplyT2 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT2 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- M.unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = accumT i m1 j m2'
      M.unsafeWrite m3 i j z
  M.unsafeFreeze m3
    where m2' = MF.transpose m2

accumT :: Int -> M.Matrix -> Int -> M.Matrix -> Double
accumT ithrow (M.Matrix r1 c1 v1) jthcol (M.Matrix _ c2 v2) = sub 0 0
  where sub !acc !ij | ij == r1 = acc
                     | otherwise = sub ( valRow*valCol + acc ) (ij+1)
                                   where 
                                    valRow = U.unsafeIndex v1 (ithrow*c1 + ij)
                                    valCol = U.unsafeIndex v2 (jthcol*c2 + ij)

multiplyT3 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT3 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- M.unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = U.sum $ U.zipWith (*) (M.row m1 i) (M.row m2' j)
      M.unsafeWrite m3 i j z
  M.unsafeFreeze m3
    where m2' = MF.transpose m2 

multiplyT4 :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT4 m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- M.unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = 0
      M.unsafeWrite m3 i j z
  M.unsafeFreeze m3
    where m2' = MF.transpose m2