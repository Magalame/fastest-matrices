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
                         C.bench "multiplication" $ C.nf (MF.multiply aDLA) bDLA
                       ],
        C.bgroup "Hmatrix" [ 
                             C.bench "multiplication" $ C.nf ((<>) aH) bH
                           ],
        C.bgroup "Massiv" [                                  
                                 C.bench "multiplication" $ C.nf ((MA.|*|) aMA) bMA
                                 ]
                  ]