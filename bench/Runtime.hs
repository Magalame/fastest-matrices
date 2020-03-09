{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}


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

import qualified Criterion.Main as C

#define N 10
#define N2 100

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
                         C.bench "1" $ C.nf (sum_vec_row . multiplyFuse bDLA) bDLA,
                         C.bench "2" $ C.nf (multiplyFused bDLA) bDLA,
                         C.bench "3" $ C.nf (multiplyFused2 bDLA) bDLA
                       ]

        -- C.env (pure (aH', bH', subH', vH')) $ \ ~(aH, bH, subH, vH) ->
        --     C.bgroup "Hmatrix" [ 
        --                      C.bench "multiplication" $ C.nf ((<>) aH) bH,
        --                      C.bench "repeated multiplication" $ C.nf ( H.sumElements . flip (H.?) [1] . (<>) bH . (<>) aH . (<>) aH) bH,
        --                      C.bench "multiplicationV" $ C.nf ((H.#>) aH) subH,
        --                      C.bench "qr factorization" $ C.nf H.qr aH,
        --                      C.bench "transpose" $ C.nf H.tr aH,
        --                      C.bench "norm" $ C.nf H.norm_2 vH,
        --                      C.bench "row" $ C.nf ((H.?) aH) [0],
        --                      C.bench "column" $ C.nf ((H.¿) aH) [0], 
        --                      C.bench "identity" $ C.nf identH n,
        --                      C.bench "diag" $ C.nf H.diag subH,
        --                      C.bench "map const 0" $ C.nf (mapH elemZero) aH,
        --                      C.bench "map sqr" $ C.nf (mapH elemSqr) aH
        --                    ],

        -- C.env (pure (aMA', bMA', vMA')) $ \ ~(aMA, bMA, vMA) ->
        --     C.bgroup "Massiv" [
        --                          C.bench "multiplication" $ C.nf ((MA.|*|) aMA) bMA,
        --                          C.bench "multiplication (Par)" $ C.nf ((MA.|*|) (MA.setComp MA.Par aMA)) bMA,
        --                          C.bench "repeated multiplication" $ C.nf ( MA.foldlS (+) 0 .  flip (MA.!>) 1 . (MA.|*|) bMA . (MA.|*|) aMA . (MA.|*|) aMA) bMA,
        --                          C.bench "repeated multiplication (Par)" $ C.nf ( MA.foldlS (+) 0 .  flip (MA.!>) 1 . (MA.|*|) bMA . (MA.|*|) aMA . (MA.|*|) (MA.setComp MA.Par aMA)) bMA,
        --                          C.bench "norm" $ C.nf (sqrt . MA.foldlS (+) 0 . (MA.zipWith (*) vMA)) vMA,
        --                          C.bench "transpose" $ C.nf (MA.computeAs MA.P . MA.transpose) aMA,
        --                          C.bench "row" $ C.nf (MA.computeAs MA.P . (MA.!>) aMA) 0,
        --                          C.bench "column" $ C.nf (MA.computeAs MA.P . (MA.<!) aMA) 0
        --                          ]
                  ]

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
