{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module Common (vDLA',uDLA',subDLA',aDLA',bDLA'
               ,vH',subH',aH',bH'
               ,vNH',aNH',bNH'
               ,aDMX',bDMX'
               ,vMA',aMA',bMA',
               ---
               identH,identDMX,elemZero,elemSqr,mapH)
    where




#define N 100
#define N2 10000


import qualified Data.Vector.Unboxed         as U
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector as V

-- data generation
import qualified System.Random.MWC as MWC
import System.IO.Unsafe (unsafePerformIO)

-- DLA
import qualified Statistics.Matrix as M

-- hmatrix
import qualified Numeric.LinearAlgebra as H

-- numhask
import qualified NumHask.Array as NH
import qualified NumHask.Prelude as NP

-- massiv
import qualified Data.Massiv.Array as MA

-- data.matrix from matrix
import qualified Data.Matrix as DMX



n :: Int
n = N

vectorGen :: Vector Double
vectorGen = unsafePerformIO $ do 
    gen <- MWC.create
    MWC.uniformVector gen (2*n*n)

-------------

vDLA',uDLA',subDLA' :: Vector Double
vDLA' = U.take (n*n) vectorGen
uDLA' = U.drop (n*n) vectorGen
subDLA' = U.take n vDLA'

aDLA',bDLA' :: M.Matrix
aDLA' = M.Matrix n n vDLA'
bDLA' = M.Matrix n n uDLA'

vList, uList :: [Double]
vList = U.toList vDLA'
uList = U.toList uDLA'

vH',subH' :: H.Vector Double
vH' = H.fromList vList
subH' = H.fromList . take n $ vList

aH',bH' :: H.Matrix Double
aH' = (n H.>< n) vList
bH' = (n H.>< n) uList

vNH' :: NH.Array V.Vector '[N2] Double
vNH' = NP.fromList vList 

aNH' :: NH.Array V.Vector '[N, N] Double
aNH' = NP.fromList vList
bNH' :: NH.Array V.Vector '[N, N] Double
bNH' = NP.fromList uList

aDMX' = DMX.fromList n n vList
bDMX' = DMX.fromList n n uList

vMA' :: MA.Array MA.P MA.Ix1 Double
vMA' = MA.fromList MA.Seq vList

aMA',bMA' :: MA.Array MA.P MA.Ix2 Double
aMA' = MA.resize' (MA.Sz (n MA.:. n)) vMA'
bMA' = MA.resize' (MA.Sz (n MA.:. n)) $ MA.fromList MA.Seq uList

-----------

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