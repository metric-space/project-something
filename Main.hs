module Main where

import Codec.Picture
import qualified Data.Map as M
import qualified Data.Vector.Storable as V
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Tuple (swap)


type Point = (Int, Int)
type InfoBit = (PixelRGB8, Point)


blank :: PixelRGB8
blank =  PixelRGB8 255 255 255


picture :: String
picture = "gintoki.jpg"

-- =================== Vector Ops ================================

changeOrigin :: Point -> Point -> Point
changeOrigin (x0, y0) (x,y) = (x - x0, y - y0)


reverseChangeOrigin :: Point -> Point -> Point
reverseChangeOrigin (x0, y0) (x,y) = (x + x0, y + y0)


calculateOrigin :: Int -> Int -> Point
calculateOrigin w h = let f = flip quot 2
                      in (f w, f h)


rotate :: Float -> Point -> Point
rotate angle (x,y) = let x_ = fromIntegral x  
                         y_ = fromIntegral y 
                     in (round (x_ * (cos angle) + y_ * (sin angle)),
                         round (-x_ * (sin angle) + y_ * (cos angle)))

-- ================= STEPS ============================



generateInfoBits :: Image PixelRGB8 -> [InfoBit]
generateInfoBits image@(Image w h _) = do
                                         x <- [0..(w-1)]  
                                         y <- [0..(h-1)] 
                                         return (pixelAt image x y, (x,y))


centrizeAndRotate :: (Int,Int) -> Float -> [InfoBit] -> [InfoBit]
centrizeAndRotate (w,h) angle = Prelude.map ((rotate angle . changeOrigin (w,h)) <$>) 


getExtremes :: [Point] -> (Int,Int,Int,Int)
getExtremes coords = let xs = map fst coords 
                         ys = map snd coords
                     in (minimum xs, minimum ys,  maximum xs , maximum ys)


rotateImage :: Image PixelRGB8 -> Image PixelRGB8
rotateImage image@(Image w h _) = let (cx,cy) = calculateOrigin w h 
                                      rotatedInfoBits = centrizeAndRotate (cx,cy) 45 . generateInfoBits $ image
                                      (minx,miny, maxx, maxy) = getExtremes . map snd $ rotatedInfoBits 
                                      mappedForm  = M.fromList . map swap . Prelude.map ((reverseChangeOrigin (-minx, -miny)) <$>) $ rotatedInfoBits
                                  in generateImage (\x y -> maybe blank id (M.lookup (x,y) mappedForm)) (maxx-minx) (maxy - miny)


rotateRGB8 :: DynamicImage -> DynamicImage
rotateRGB8  = ImageRGB8 . rotateImage . convertRGB8  


main :: IO ()
main = do
         x <- readJpeg picture
         case x of Right y -> saveJpgImage 100 "rotate.jpg" (rotateRGB8 y)
                   Left _ -> putStrLn "Loaded not!"
