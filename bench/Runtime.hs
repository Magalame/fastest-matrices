{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}


module Main where



import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V

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
import Data.Massiv.Core.Common

import qualified System.Random.MWC as Mwc

import qualified Criterion.Main as C

import Control.Monad.ST


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
      vMA = MA.fromList MA.Seq vList :: MA.Array MA.P MA.Ix1 Double
      aMA = MA.resize' (MA.Sz $ MA.Ix2 n n) vMA :: MA.Array MA.P MA.Ix2 Double
      bMA = MA.resize' (MA.Sz $ MA.Ix2 n n) $ MA.fromList MA.Seq uList :: MA.Array MA.P MA.Ix2 Double

    C.defaultMain [ 
        C.bgroup "DLA" [ 
                         C.bench "multiplication" $ C.nf (multiplyT aDLA) bDLA
                         ,C.bench "multiplyEmpty" $ C.nf (multiplyEmpty aDLA) bDLA
                         ,C.bench "transpose" $ C.nf M.transpose aDLA

                         ,C.bench "dot" $ C.nf (U.sum . U.zipWith (*) (M.row aDLA 0)) (M.row bDLA 1)
                       ],
        -- C.bgroup "Hmatrix" [ 
        --                      C.bench "multiplication" $ C.nf ((<>) aH) bH

        --                    ],
        C.bgroup "Massiv" [ 
                                 C.bench "multiplication" $ C.nf ((MA.|*|) aMA) bMA
                                 ,C.bench "multiplyEmpty" $ C.nf ((MA.>*<) aMA) bMA
                                 ,C.bench "transpose" $ C.nf (MA.computeAs MA.P . MA.transpose) aMA
                                 ,C.bench "dot" $ C.nf (MA.foldlS (+) 0 . MA.zipWith (*) (unsafeOuterSlice aMA 0)) (unsafeOuterSlice bMA 1)

                                 ]
                  ]

multiplyT :: M.Matrix -> M.Matrix -> M.Matrix
multiplyT m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- M.unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = U.sum $ U.zipWith (*) (M.row m1 i) (M.row m2' j)
      M.unsafeWrite m3 i j z
  M.unsafeFreeze m3
    where m2' = transpose m2 

multiplyEmpty :: M.Matrix -> M.Matrix -> M.Matrix
multiplyEmpty m1@(M.Matrix r1 _ _) m2@(M.Matrix _ c2 _) = runST $ do
  m3 <- M.unsafeNew r1 c2
  M.for 0 c2 $ \j -> do
    M.for 0 r1 $ \i -> do
      let 
        z = 0
      M.unsafeWrite m3 i j z
  M.unsafeFreeze m3
    where m2' = transpose m2 


transpose :: M.Matrix -> M.Matrix
transpose (M.Matrix r0 c0 v0) 
  = M.Matrix c0 r0 $ runST $ do
    vec <- UM.unsafeNew (r0*c0)
    M.for 0 r0 $ \i -> do
      UM.unsafeWrite vec (i + i * c0) $ v0 `U.unsafeIndex` (i + i * c0)
      M.for (i+1) c0 $ \j -> do
        let tmp = v0 `U.unsafeIndex` (j + i * c0)
            tmp2 = v0 `U.unsafeIndex` (i + j * c0)
        UM.unsafeWrite vec (j + i * c0) tmp2
        UM.unsafeWrite vec (i + j * c0) tmp
    U.unsafeFreeze vec