module Handler.About
       ( getAboutR
       , getCVR
       ) where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $(widgetFile "about")

getCVR :: Handler RepHtml
getCVR = defaultLayout $(widgetFile "cv")
