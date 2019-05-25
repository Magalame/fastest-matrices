{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}


module Main where



import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector as V

-- DLA
import qualified Statistics.Matrix as M
import           Statistics.Matrix (Matrix (..))
import qualified Statistics.Matrix.Algorithms as A

-- hmatrix
import qualified Numeric.LinearAlgebra as H

-- numhask
import qualified NumHask.Array as NH
import qualified NumHask.Prelude as NP

-- data.matrix from matrix
import qualified "matrix" Data.Matrix as DMX

-- bed and breakfast
import qualified Numeric.Matrix as NM

import qualified System.Random.MWC as Mwc

import qualified Weigh as W

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

deriving instance Generic Matrix
deriving instance NFData Matrix

n :: Int
n = 50

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

identNM :: Int -> NM.Matrix Double
identNM = NM.unit

elemZero :: Double -> Double
elemZero = const 0

elemSqr :: Double -> Double
elemSqr x = x*x

mapH :: (Double -> Double) -> H.Matrix Double -> H.Matrix Double
mapH = H.cmap

main :: IO ()
main = do 

    let 

    --
      vDLA = U.fromList $ take (n*n) $ [1 :: Double ..] :: Vector Double
      uDLA = U.fromList $ take (n*n) $ [0 :: Double ..] :: Vector Double
      subDLA = U.fromList $ take (n) $ [1 :: Double ..] :: Vector Double

      aDLA = M.Matrix n n vDLA
      bDLA = M.Matrix n n uDLA
    
    --
      vList = U.toList vDLA :: [Double]
      uList = U.toList uDLA :: [Double]
      subList = U.toList subDLA :: [Double]
    
    --
      aH = (n H.>< n) vList
      bH = (n H.>< n) uList

      subH = H.fromList subList

    --
      aNH = [1 ..] :: NH.Array V.Vector '[50, 50] Double
      bNH = [0 ..] :: NH.Array V.Vector '[50, 50] Double
      subNH = [1 ..] :: NH.Array V.Vector '[50] Double

    --
      aDMX = DMX.fromList n n vList
      bDMX = DMX.fromList n n uList
    --

    W.mainWith (do 
               W.func "DLA - multiplication" (M.multiply aDLA) bDLA
               W.func "DLA - qr factorization" A.qr aDLA
               W.func "DLA - transpose" M.transpose aDLA
               W.func "DLA - norm" M.norm subDLA
               W.func "DLA - row" (M.row  aDLA) 0
               W.func "DLA - column" (M.column  aDLA) 0
               W.func "DLA - identity" M.ident n

               W.func "Hmatrix - multiplication" ((<>) aH) bH
               W.func "Hmatrix - qr factorization" H.qr aH
               W.func "Hmatrix - transpose" H.tr aH
               W.func "Hmatrix - norm" H.norm_2 subH
               W.func "Hmatrix - row" ((H.?) aH) [0]
               W.func "Hmatrix - column" ((H.¿) aH) [0]
               W.func "Hmatrix - identity" identH n

               W.func "NumHask Array - multiplication" (NH.mmult aNH) bNH
               W.func "NumHask Array - transpose" NH.transpose aNH
               W.func "NumHask Array - norm" (sqrt . (NP.<.> subNH)) subNH
               W.func "NumHask Array - row" (NH.row (NP.Proxy :: NP.Proxy 0)) aNH
               W.func "NumHask Array - column" (NH.col (NP.Proxy :: NP.Proxy 0)) aNH

               W.func "matrix - multiplication" (DMX.multStrassenMixed aDMX) bDMX
               W.func "matrix - transpose" DMX.transpose aDMX
               W.func "matrix - row" (DMX.getRow 1) aDMX
               W.func "matrix - column" (DMX.getCol 1) aDMX
               W.func "matrix - identity" identDMX n
               )