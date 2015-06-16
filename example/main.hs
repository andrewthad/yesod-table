{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Minimal where

import Data.Monoid
import Data.Functor.Contravariant ((>$<))
import Control.Applicative

import Yesod
import qualified Yesod.Table as Table
import Yesod.Table (Table)
import Network.Wai.Handler.Warp (run)

import Data.Text (Text)
import qualified Data.Text as Text

data App = App
instance Yesod App 

mkYesod "App" [parseRoutes|
  / RootR GET
|]

data Person = Person
  { firstName     :: Text
  , lastName      :: Text
  , age           :: Int
  , favoriteColor :: Color
  }

data Dog = Dog
  { breed     :: Text
  , dogName   :: Text
  }

data Color = Red | Green | Blue | Purple
  deriving (Show)

widgetizeColor :: Color -> Widget
widgetizeColor c = [whamlet|<span style="color:#{getHexVal c}">#{show c}|]
  where getHexVal color = case color of
          Red -> "#F00" :: Text
          Green -> "#0F0"
          Blue -> "#00F"
          Purple -> "#800080"

peopleTable :: Table App Person
peopleTable = mempty
  <> Table.text    "First Name"            firstName
  <> Table.text    "Last Name"             lastName
  <> Table.int     "Age"                   age
  <> Table.text    "Favorite Color Name"   (Text.pack . show . favoriteColor)
  <> Table.widget  "Favorite Color Sample" (widgetizeColor . favoriteColor)

dogTable :: Table App Dog
dogTable = mempty
  <> Table.text "Breed"    breed
  <> Table.text "Dog Name" dogName

people :: [Person]
people = [ Person "Drew" "Martin" 24 Red
         , Person "Alexa" "Martin" 23 Purple
         , Person "Jordan" "Nolan" 36 Green
         ]

dogs :: [Dog]
dogs = [ Dog "Terrier" "Sparky"
       , Dog "Shelty" "Trixie"
       , Dog "Great Dane" "Gnarles Barkley"
       ]

getRootR :: Handler Html
getRootR = defaultLayout $ do
  addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
  setTitle "Yesod Tables"
  let p  = Table.buildBootstrap peopleTable people
      d  = Table.buildBootstrap dogTable dogs
      pd = Table.buildBootstrap -- The (>$<) operator is from Data.Functor.Contravariant
             ((fst >$< peopleTable) <> (snd >$< dogTable))
             (liftA2 (,) people dogs)
  [whamlet|
    <div.container>
      <h1>Table of People
      ^{p}
      <h1>Table of Dogs
      ^{d}
      <h1>Cross Join of People and Dogs
      ^{pd}
  |]
  
main :: IO ()
main = run 3000 =<< toWaiApp App
