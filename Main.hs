{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom

main = mainWidget $ mdo
         (element, _) <- el' "h1" $ dynText $ fmap (\n -> if n > 0 then "Clicked" else "Hello World") nClicks
         nClicks <- foldDyn (\_ -> succ) 0 $ domEvent Click element 
         el "p" $ text "This is my test document."
