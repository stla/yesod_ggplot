{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation ( Route(GgplotR, StaticR, HomeR), App(getStatic), resourcesApp )
import Yesod.Core ( mkYesodDispatch )
import Home       ( getHomeR )
import Ggplot     ( getGgplotR, putGgplotR )

mkYesodDispatch "App" resourcesApp
