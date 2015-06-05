module Yesod.Table 
  ( Table(..)
  , Column(..)
  , buildBootstrap
  , single
  , text
  , linked
  ) where

import Prelude hiding (mapM_)
import Yesod.Core
import Yesod.Core.Widget
import Data.Functor.Contravariant
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

-- import Control.Monad
import Data.Foldable (forM_, mapM_)
import Data.Monoid

data Column site a = Column
  { header :: !(WidgetT site IO ())
  , cell :: !(a -> WidgetT site IO ()) 
  }

newtype Table site a = Table (Seq (Column site a))
  deriving (Monoid)

instance Contravariant (Column site) where
  contramap g (Column h c) = Column h (c . g)

instance Contravariant (Table site) where
  contramap g (Table cols) = Table (fmap (contramap g) cols)

single :: WidgetT site IO () -> (a -> WidgetT site IO ()) -> Table site a
single c h = Table (Seq.singleton (Column c h))

text :: Text -> (a -> Text) -> Table site a
text h c = single (textToWidget h) (textToWidget . c)

linked :: Text -> (a -> Text) -> (a -> Route site) -> Table site a
linked h propFunc routeFunc = single (textToWidget h) render
  where render a = [whamlet|<a href=@{routeFunc a}>#{propFunc a}|]

buildBootstrap :: Table site a -> [a] -> WidgetT site IO ()
buildBootstrap (Table cols) vals = table $ do
  thead $ mapM_ (td . header) cols
  tbody $ forM_ vals $ \val -> tr $ forM_ cols $ \col -> td $ cell col val
  where table b  = [whamlet|
                     <table.table.table-striped>^{b}
                   |]
        thead b  = [whamlet|
                     <thead>
                       <tr>
                         ^{b}
                   |]
        td b     = [whamlet|
                     <td>^{b}
                   |]
        tbody b  = [whamlet|
                     <tbody>^{b}
                   |]
        tr b     = [whamlet|
                     <tr>^{b}
                   |]

textToWidget :: Text -> WidgetT site IO ()
textToWidget = toWidget . toHtml

