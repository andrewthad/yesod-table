yesod-table
===========

#### What is yesod-table?
This is a library that provides an abstraction for building HTML tables. A table is 
parameterized over a foundation type `site` and any type `a`. The table is a collection
of columns, where each column has two parts:
  1. A title (`WidgetT site IO ()`)
  2. A function for visualizing the data (`a -> WidgetT site IO ()`)

#### Where can I find a working example?
In the examples folder, the is a single-file yesod application that demonstrates 
how yesod-table works. It first show how the `Monoid` instance is used to build
smaller pieces into a larger table, and then it combines those table, making 
use of the `Contravariant` instance.

#### How can I use this?
Here is a simple example:

    import Yesod
    import qualified Yesod.Table as Table
    import Yesod.Table (Table)
    import Data.Text (Text)

    data Person = Person
      { firstName :: Text
      , lastName  :: Text
      , age       :: Int
      }
    
    myTable :: Table App Person
    myTable = mempty
      <> Table.text "First Name" firstName
      <> Table.text "Last Name"  lastName
      <> Table.int  "Age"        age

Then, you can render the table by providing it with rows of data. So, in a handler
somewhere, you could write:

    getHomeR :: Handler Html
    getHomeR = defaultLayout $ Table.buildBootstrap myTable 
      [ Person "Drew" "Martin" 23
      , Person "Alexa" "Martin" 23
      ]

The home page would have a table with two rows.

