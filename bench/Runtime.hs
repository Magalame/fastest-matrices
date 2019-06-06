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
import qualified Statistics.Matrix.Fast as MF
import qualified Statistics.Matrix.Fast.Algorithms as A

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

import qualified Criterion.Main as C

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

    --
      aNH = NP.fromList vList :: NH.Array V.Vector '[50, 50] Double
      bNH = NP.fromList uList :: NH.Array V.Vector '[50, 50] Double

      subNH = NP.fromList . take n $ vList :: NH.Array V.Vector '[50] Double

    --
      aDMX = DMX.fromList n n vList
      bDMX = DMX.fromList n n uList

    C.defaultMain [ 
        C.bgroup "DLA" [ 
                         C.bench "multiplication" $ C.nf (MF.multiply aDLA) bDLA,
                         C.bench "repeated multiplication" $ C.nf (U.sum . (flip M.row) 1 . MF.multiply bDLA . MF.multiply aDLA . MF.multiply aDLA ) bDLA,
                         C.bench "multiplicationV" $ C.nf (MF.multiplyV aDLA) subDLA,

                         C.bench "qr factorization" $ C.nf A.qr aDLA,

                         C.bench "transpose" $ C.nf M.transpose aDLA,

                         C.bench "norm" $ C.nf MF.norm vDLA,
                         C.bench "row" $ C.nf (M.row  aDLA) 0,
                         C.bench "column" $ C.nf (M.column  aDLA) 0,

                         C.bench "identity" $ C.nf M.ident n, 
                         C.bench "diag" $ C.nf M.diag subDLA, 
                         C.bench "map const 0" $ C.nf (M.map elemZero) aDLA,
                         C.bench "map sqr" $ C.nf (M.map elemSqr) aDLA
                       ],
        C.bgroup "Hmatrix" [ 
                             C.bench "multiplication" $ C.nf ((<>) aH) bH,
                             C.bench "repeated multiplication" $ C.nf ( H.sumElements . flip (H.?) [1] . (<>) bH . (<>) aH. (<>) aH) bH,
                             C.bench "multiplicationV" $ C.nf ((H.#>) aH) subH,

                             C.bench "qr factorization" $ C.nf H.qr aH,

                             C.bench "transpose" $ C.nf H.tr aH,

                             C.bench "norm" $ C.nf H.norm_2 subH,
                             C.bench "row" $ C.nf ((H.?) aH) [0],
                             C.bench "column" $ C.nf ((H.¿) aH) [0], 

                             C.bench "identity" $ C.nf identH n,
                             C.bench "diag" $ C.nf H.diag subH,
                             C.bench "map const 0" $ C.nf (mapH elemZero) aH,
                             C.bench "map sqr" $ C.nf (mapH elemSqr) aH
                           ],
        C.bgroup "NumHask" [ 
                             C.bench "multiplication" $ C.nf (NH.mmult aNH) bNH,
                             C.bench "repeated multiplication" $ C.nf ( (\(NH.Array a) -> V.sum a) . NH.row (NP.Proxy :: NP.Proxy 1) . NH.mmult bNH . NH.mmult aNH . NH.mmult aNH ) bNH,

                             C.bench "transpose" $ C.nf NH.transpose aNH,

                             C.bench "norm" $ C.nf (sqrt . (NP.<.> subNH)) subNH,

                             C.bench "row" $ C.nf (NH.row (NP.Proxy :: NP.Proxy 0)) aNH,
                             C.bench "column" $ C.nf (NH.col (NP.Proxy :: NP.Proxy 0)) aNH
                           ],

        C.bgroup "Matrix" [ 
                                 C.bench "multiplication" $ C.nf (DMX.multStrassenMixed aDMX) bDMX,
                                 C.bench "transpose" $ C.nf DMX.transpose aDMX,
                                 C.bench "row" $ C.nf (DMX.getRow 1) aDMX,
                                 C.bench "column" $ C.nf (DMX.getCol 1) aDMX,
                                 C.bench "identity" $ C.nf identDMX n
                                 ]
                  ]
