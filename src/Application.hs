{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core ( mkYesodDispatch )
import Home       ( getHomeR )
import Gglot

mkYesodDispatch "App" resourcesApp
