{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Accelerate (
    computeEscapes
) where

import Data.Array.Accelerate                              as A hiding ( fromInteger )
import Data.Array.Accelerate.Data.Complex                 as A

import Prelude (fromInteger)
import qualified Prelude as P

computeEscapes :: forall a. (Num a, RealFloat a, FromIntegral Int a, Elt (Complex a))
               => Int -- ^ Image width
               -> Int -- ^ Image height
               -> Acc (Scalar a) -- ^ Center X
               -> Acc (Scalar a) -- ^ Center Y
               -> Acc (Scalar a) -- ^ Magnification
               -> Acc (Scalar Int32) -- ^ Iteration Limit
               -> Acc (Scalar a)     -- ^ Divergence radius
               -> Acc (Array DIM2 (Complex a, Int32))
computeEscapes w h (the -> cx) (the -> cy) (the -> mg) (the -> limit) (the -> radius) = 
    generate (constant (Z :. h :. w))
             (\ix -> let (Z :. y :. x) = unlift ix :: (Z :. Exp Int) :. Exp Int
                         aspectRatio = (P.fromIntegral w) / (P.fromIntegral h) :: Exp a
                         rw = if aspectRatio <= 4.0 / 3.0 then 4.0 / mg else 3.0 / mg * aspectRatio
                         rh = if aspectRatio <= 4.0 / 3.0 then 4.0 / mg / aspectRatio else 3.0 / mg
                         re = cx - (rw / 2.0) + ((fromIntegral x + 0.5) / fromIntegral (constant w)) * rw
                         im = cy - (rh / 2.0) + ((fromIntegral y + 0.5) / fromIntegral (constant h)) * rh

                         z0 = lift (re :+ im) :: Exp (Complex a)
                         zn = while (\zi -> snd zi < limit && dot (fst zi) < radius)
                                    (\zi -> step z0 zi)
                                    (lift (z0, constant 0))
                     in zn)
    where
        dot :: Exp (Complex a) -> Exp a
        dot (unlift -> x :+ y) = x*x + y*y

        step :: Exp (Complex a) -> Exp (Complex a, Int32) -> Exp (Complex a, Int32)
        step c (unlift -> (z, i)) = lift (c + z * z, i + constant 1)