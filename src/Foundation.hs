{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core   ( mkYesodData
                    , parseRoutesFile
                    , Yesod
                    , RenderRoute(renderRoute) )
import Yesod.Static ( staticFiles, Static )
staticFiles "static"

newtype App = App { getStatic :: Static }

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App
