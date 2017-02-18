{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-} -- allows for local type declarations.
import Reflex
import Reflex.Dom
import Data.Text (Text, pack)
import Data.Map (Map)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

webPage :: MonadWidget t m => m ()
webPage = do

  -- ticker Event fires once per second.
  ticker :: Event t TickInfo <- tickLossy 1.0 =<< liftIO getCurrentTime  

  -- counter Dynamic increases by one every second.
  counter :: Dynamic t Int <- foldDyn  (\_ n -> n+1) 0 ticker

  -- function to map from integer to red or blue style.
  let chooseColor :: Int -> (Map Text Text) 
      chooseColor n = "style" =: pack ("color: " ++ if (n `mod` 2) == 0 then "red" else "blue")
 
  -- redBlueStyle Dynamic changes each second.
  let redBlueStyle :: Dynamic t (Map Text Text) 
      redBlueStyle = fmap chooseColor counter

  -- insert an h1 elemnt.
  el "h1" $ text "Hello World!"

  -- insert a paragraph with Dynamic red blue style.
  elDynAttr "p"  redBlueStyle $ text "This is my test document"

  return ()


css = "h1 {font-family: Helvetica;} p {font-family: Helvetica;}" 

main :: IO ()
main = mainWidgetWithCss css webPage
