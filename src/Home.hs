{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation ( Route(GgplotR), Handler )
import Yesod.Core ( whamlet, setTitle, Yesod(defaultLayout), Html )

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Menu"
    [whamlet|
        <p>
            <a href=@{GgplotR}>Upload a file and plot
    |]
