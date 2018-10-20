{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( genImage
    , ViewPort(..)
    ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Fixed (mod')
import Data.Word (Word8)
import Data.Array
import Data.Hashable
import Data.Complex
import Control.Parallel.Strategies
import GHC.Generics

data ViewPort = ViewPort { _centerX :: Double 
                         , _centerY :: Double
                         , _magnification :: Double
                         , _maxIterations :: Int
                         , _rotation :: Double
                         , _superScaling :: Int
                         } deriving (Eq, Show, Generic)
                         
instance Hashable ViewPort
                        
genImage :: Int -> Int -> ViewPort -> DynamicImage
genImage w h v = ImageRGB8 $ generateImage (renderImage v w h) w h

renderImage :: ViewPort -> Int -> Int -> (Int -> Int -> PixelRGB8)
renderImage (ViewPort cx cy mg mi _ s) w h = 
    let aspectRatio = fromIntegral w / fromIntegral h :: Double
        (rw, rh) = if aspectRatio <= 4.0 / 3.0 then (4.0 / mg, 4.0 / mg / aspectRatio)
                                               else (3.0 / mg * aspectRatio, 3.0 / mg)
        f x y = let ps = [(x * s + ix, y * s + iy) | ix <- [0..s-1], iy <- [0..s-1]]
                    r x' = cx - (rw / 2.0) + ((fromIntegral x' + 0.5) / fromIntegral (w * s) :: Double) * rw
                    i y' = cy - (rh / 2.0) + ((fromIntegral y' + 0.5) / fromIntegral (h * s) :: Double) * rh
                    rs = fmap (\(x', y') -> (r x', i y')) ps
                    qs = fmap (\(r0, i0) -> color (fastRender (realToFrac r0 :+ realToFrac i0) mi)) rs
                    (rc, gc, bc) = foldr (\(PixelRGB8 r g b) (rc :: Int, gc :: Int, bc :: Int) -> (rc + fromIntegral r, gc + fromIntegral g, bc + fromIntegral b)) (0, 0, 0) qs
                 in PixelRGB8 (fromIntegral (rc `div` (s*s))) (fromIntegral (gc `div` (s*s))) (fromIntegral (bc `div` (s*s)))  
    in f

mag2 :: (RealFloat a, Num a) => Complex a -> a
mag2 (r :+ i) = r*r + i*i

render :: (RealFloat a, Num a) => Complex a -> Int -> Maybe (Complex a, Int)
render c maxI = loop (0 :+ 0) 0
    where loop z i = if mag2 z <= 4 && i <= maxI
                        then loop (step z) (i + 1)
                        else if i < maxI
                            then Just (step $ step z, i)
                            else Nothing
          step z = z*z + c

fastRender :: (RealFloat a, Num a) => Complex a -> Int -> Maybe (Complex a, Int)
fastRender c maxI = fastLoop (0 :+ 0) 0
    where fastLoop z i = let zn = (step . step . step . step . step . step . step . step) z
                         in if mag2 zn <= 4 && (i + 8) <= maxI
                            then fastLoop zn (i + 8)
                            else slowLoop z i
          slowLoop z i = if mag2 z <= 4 && i <= maxI
                         then slowLoop (step z) (i + 1)
                         else if i < maxI
                              then Just (step $ step z, i)
                              else Nothing
          step z = z*z + c

color :: (RealFloat a, Num a) => Maybe (Complex a, Int) -> PixelRGB8
color Nothing = PixelRGB8 0 0 0
color (Just (z, i)) = let mu = ((fromIntegral (i + 1)) - (logBase 2 . log . mag2) z) / 80.0
                          frac = snd (properFraction mu)
                      in getColor $ if frac < 0 then frac + 1.0 else frac
{-
renderPoint :: Double     -- ^ The real coordinate
            -> Double     -- ^ The imaginary coordinate
            -> Int        -- ^ The max iterations
            -> PixelRGB8  -- ^ The resulting pixel
renderPoint r0 i0 mi = case loop (0, 0) 0 of
                       Left (p, iter) -> let (r2, i2) = step . step $ p
                                             mu = ((fromIntegral (iter + 1)) - (logBase 2 . log) (r2 * r2 + i2 * i2)) / 80.0
                                             frac = snd (properFraction mu)
                                         in getColor $ if frac < 0 then frac + 1.0 else frac
                       Right _ -> PixelRGB8 0 0 0
    where loop (r, i) iter = if r*r + i*i <= 4 && iter <= mi
                          then loop (step (r, i)) (iter + 1)
                          else if iter < mi
                               then Left ((r, i), iter)
                               else Right ()
          step (r, i) = (r*r - i*i + r0, 2*r*i + i0)
-}

palette :: (RealFloat a, Num a) => [(a, PixelRGB8)]
palette = [ (0.0,    PixelRGB8 0   7   100)
          , (0.16,   PixelRGB8 32  107 203)
          , (0.42,   PixelRGB8 237 255 255)
          , (0.6425, PixelRGB8 255 170 0)
          , (0.8575, PixelRGB8 0   2   0)
          , (1.0,    PixelRGB8 0   7   100)
          ]

getColor :: (RealFloat a, Num a) => a -> PixelRGB8
getColor f = let ((li, (PixelRGB8 lr lg lb)), (ri, (PixelRGB8 rr rg rb))) = pair palette
                 index = (f - li) / (ri - li)
                 index2 = index * index
                 index3 = index * index2
                 r = fromIntegral . floor $ (2.0 * (fromIntegral lr - fromIntegral rr) * index3) - (3.0 * (fromIntegral lr - fromIntegral rr) * index2) + fromIntegral lr
                 g = fromIntegral . floor $ (2.0 * (fromIntegral lg - fromIntegral rg) * index3) - (3.0 * (fromIntegral lg - fromIntegral rg) * index2) + fromIntegral lg
                 b = fromIntegral . floor $ (2.0 * (fromIntegral lb - fromIntegral rb) * index3) - (3.0 * (fromIntegral lb - fromIntegral rb) * index2) + fromIntegral lb
             in PixelRGB8 r g b
    where pair (c:cs) = if f >= fst (head cs) then pair cs else (c, head cs)