{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib
import Accelerate
import Data.Array.Accelerate
{- import Data.Array.Accelerate.Interpreter as I -}

import Data.Proxy
import Data.Hashable
import Servant
import Network.Wai.Handler.Warp
import Lucid
import Servant.HTML.Lucid
import Servant.JuicyPixels
import Codec.Picture (DynamicImage, savePngImage)

import Data.Text

type SplendorApi = "game" :> Get '[HTML] (Html ())
              :<|> "render" :> QueryParam "w" Int :> QueryParam "h" Int :> QueryParam "x" Double :> QueryParam "y" Double :> QueryParam "m" Double :> QueryParam "i" Int :> QueryParam "s" Int :> Get '[PNG] DynamicImage
              :<|> "static" :> Raw

input :: Text -> Text -> Text -> Html ()
input l n d = div_ [] $ do
    tr_ [] $ do
        td_ [] (toHtml l)
        td_ [] $ do
            input_ [type_ "text", name_ n, value_ d]        

home :: Html ()
home = do
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                script_ [type_ "text/javascript", src_ "static/jquery-3.3.1.min.js"] ""
                script_ [type_ "text/javascript", src_ "static/site.js"] ""
                link_ [href_ "static/site.css", type_ "text/css", rel_ "stylesheet"]
            body_ $ do
                table_ [id_ "layout"] $ do
                    tr_ [] $ do
                        td_ [] $ do
                            form_ [method_ "get", action_ "/render", target_ "_blank", id_ "form"] $ do
                                table_ [id_ "preview"] $ do
                                    input "Center X: " "x" "-0.5"
                                    input "Center Y: " "y" "0.0"
                                    input "Magnification: " "m" "1.0"
                                    input "Iterations: " "i" "100"
                                table_ [id_ "generate"] $ do
                                    input "Width: " "w" "3300"
                                    input "Height: " "h" "2550"
                                    input "Super Scaling: " "s" "3"                                
                                    td_ $ do
                                        input_ [id_ "generate", type_ "button", value_ "Generate"]
                                    td_ $ do
                                        textarea_ [id_ "output", width_ "20", height_ "5"] ""
                        td_ [id_ "mandelbrot", width_ "100%"] $ do
                            img_ [src_ "", id_ "render"]

renderImago :: Maybe Int -> Maybe Int -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Int -> Maybe Int -> Handler DynamicImage
renderImago (Just w) (Just h) (Just x) (Just y) (Just m) (Just i) (Just s) = return $ genImage w h (ViewPort x y m i 0.0 s)
renderImago _ _ _ _ _ _ _ = undefined

renders = [{- ViewPort { _centerX = -0.5, _centerY = 0.0, _magnification = 1.0, _maxIterations = 100, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -1.4443297602233807, _centerY = -0.00008299170756663375, _magnification = 125949925, _maxIterations = 2000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = 0.3913891731564342, _centerY = -0.144168402513167, _magnification = 70, _maxIterations = 2000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = 0.39070136666158606, _centerY = -0.15357165538064022, _magnification = 6054.8291015625, _maxIterations = 2000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = 0.3936832534110479, _centerY = -0.13587079564545632, _magnification = 349151.95665836334, _maxIterations = 3000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -0.7421611092507143, _centerY = -0.13858353934084724, _magnification = 250, _maxIterations = 3000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -0.7448250324331561, _centerY = -0.14032153659736302, _magnification = 6407.2265625, _maxIterations = 10000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -0.7451435844834869, _centerY = -0.1386710638994096, _magnification = 831314.1825199127, _maxIterations = 5000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -0.7433389815266208, _centerY = -0.13688585585185586, _magnification = 32436.58447265625, _maxIterations = 50000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -0.08878527502624324, _centerY = -0.6547972605299416, _magnification = 4987.885095119476, _maxIterations = 3000, _rotation = 0, _superScaling = 3 }
          , ViewPort { _centerX = -0.08865398356152798, _centerY = -0.6552346845068325, _magnification = 970739.7373664756, _maxIterations = 5000, _rotation = 0, _superScaling = 3 }
          , -} ViewPort { _centerX = 0.3750001200618655, _centerY = -0.2166393884377127, _magnification = 112499998, _maxIterations = 600000, _rotation = 0, _superScaling = 3 }
          ]

server :: Server SplendorApi
server = return home
    :<|> renderImago
    :<|> serveDirectoryFileServer "C:\\Users\\micah\\Source\\DeepBlue\\src\\Static"

main :: IO ()
main = do
    mapM_ (\v -> savePngImage ("C:\\Users\\micah\\Desktop\\Images\\" Prelude.++ (show (hash v) Prelude.++ ".png")) (genImage 1000 800 v)) renders
    run 8081 (serve (Proxy :: Proxy SplendorApi) server)
    {- putStrLn . show . I.run $ computeEscapes 1600 900 (unit (lift (0.5 :: Double))) (unit (lift (0 :: Double))) (unit (lift (1 :: Double))) (unit (lift (1000 :: Int32))) (unit (lift (4 :: Double))) -}
