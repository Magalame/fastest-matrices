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
               W.func "DLA - multiplication" (MF.multiply aDLA) bDLA
               W.func "DLA - qr factorization" A.qr aDLA
               W.func "DLA - transpose" M.transpose aDLA
               W.func "DLA - norm" MF.norm vDLA
               W.func "DLA - row" (M.row  aDLA) 0
               W.func "DLA - column" (M.column  aDLA) 0
               W.func "DLA - identity" M.ident n

               W.func "Hmatrix - multiplication" ((<>) aH) bH
               W.func "Hmatrix - qr factorization" H.qr aH
               W.func "Hmatrix - transpose" H.tr aH
               W.func "Hmatrix - norm" H.norm_2 vH
               W.func "Hmatrix - row" ((H.?) aH) [0]
               W.func "Hmatrix - column" ((H.¿) aH) [0]
               W.func "Hmatrix - identity" identH n

               W.func "NumHask Array - multiplication" (NH.mmult aNH) bNH
               W.func "NumHask Array - transpose" NH.transpose aNH
               W.func "NumHask Array - norm" (sqrt . (NP.<.> vNH)) vNH
               W.func "NumHask Array - row" (NH.row (NP.Proxy :: NP.Proxy 0)) aNH
               W.func "NumHask Array - column" (NH.col (NP.Proxy :: NP.Proxy 0)) aNH

               W.func "Massiv - multiplication" ((MA.|*|) aMA) bMA
               W.func "Massiv - multiplication (Par)" ((MA.|*|) (MA.setComp MA.Par aMA)) bMA
               W.func "Massiv - norm" (sqrt . MA.foldlS (+) 0 . MA.zipWith (*) vMA) vMA
               W.func "Massiv - transpose" (MA.computeAs MA.P . MA.transpose) aMA
               W.func "Massiv - row" (MA.computeAs MA.P . (MA.!>) aMA) 0
               W.func "Massiv - column" (MA.computeAs MA.P . (MA.<!) aMA) 0

               W.func "matrix - multiplication" (DMX.multStrassenMixed aDMX) bDMX
               W.func "matrix - transpose" DMX.transpose aDMX
               W.func "matrix - row" (DMX.getRow 1) aDMX
               W.func "matrix - column" (DMX.getCol 1) aDMX
               W.func "matrix - identity" identDMX n
               )
