{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Data.Map
import Data.Text
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)


webPage :: MonadWidget t m => m ()
webPage = do
  now <- liftIO getCurrentTime 
  ticker <- tickLossy 1.0 now
  counter <- foldDyn  (\_ n -> n+1) (0::Integer) ticker
  let redBlue = fmap (\n -> if ((n `mod` 2) == 0) then "red" else "blue") $ counter
  let redBlueStyle = fmap (\color -> (fromList [(pack "style",pack $ "color: " ++ color)])) redBlue
  elDynAttr' "h1"  (constDyn $ fromList [("style","color: blue")]) $ text "Hello World!"
  elDynAttr' "p"  redBlueStyle $ text "This is my test document"
  return ()

main :: IO ()
main = mainWidgetWithCss "h1 {font-family: Helvetica;} p {font-family: Helvetica;}" webPage
