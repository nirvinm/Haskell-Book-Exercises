{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module FingerDB where

import Control.Exception
import Control.Monad (forever)

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
       (decodeUtf8, encodeUtf8)
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Database.SQLite.Simple
       hiding (close)
import qualified Database.SQLite.Simple
       as SQLite
import Database.SQLite.Simple.Types

import Text.RawString.QQ


data User = 
    User {
        userId :: Integer
      , userName :: Text
      , shell :: Text
      , homeDirectory :: Text
      , realName :: Text
      , phone :: Text
    } deriving (Eq, Show)


instance FromRow User where
    fromRow = User <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field


instance ToRow User where
    toRow (User id_ username shell
                homeDir realName phone) =
        toRow (id_, username, shell,
                homeDir, realName, phone)


createUsersQuery :: Query
createUsersQuery = [r|
CREATE TABLE IF NOT EXISTS users
   (id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE,
    shell TEXT,
    homeDirectory TEXT,
    realName TEXT,
    phone TEXT)
|]

insertUserQuery :: Query
insertUserQuery = [r|
INSERT INTO users
VALUES (?, ?, ?, ?, ?, ?)
|]

updateUserQuery :: Query
updateUserQuery = [r|
UPDATE users
SET username      = :username
  , shell         = :shell
  , homeDirectory = :homeDirectory
  , realName      = :realName
  , phone         = :phone
WHERE
    username      = :oldUsername
|]

allUsersQuery :: Query
allUsersQuery =
    "SELECT  * FROM users"


getUserQuery :: Query
getUserQuery =
    "SELECT * FROM users WHERE username = ?"


data DuplicateData =
    DuplicateData
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow =
    (Null, Text, Text, Text, Text, Text)


createDatabase :: String -> IO ()
createDatabase name = do
    conn <- open name
    execute_ conn createUsersQuery
    execute conn insertUserQuery meRow

    rows <- query_ conn allUsersQuery
    mapM_ print (rows :: [User])
    SQLite.close conn

    where meRow :: UserRow
          meRow =
            (Null, "callen", "/bin/zsh",
              "/home/callen", "Chris Allen",
              "555-123-4567")


getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn allUsersQuery


getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do
    results <- 
        query conn getUserQuery (Only username)
    case results of
        []     -> return $ Nothing
        [user] -> return $ Just user
        _      -> throwIO DuplicateData


addUser :: Connection
        -> UserRow
        -> IO ()
addUser conn usrRow =
    execute conn insertUserQuery usrRow


updateUser :: Connection
           -> Text
           -> UserRow
           -> IO ()
updateUser conn
           oldUsername
           (_ , username, shell,
            homeDirectory, realName, phone) =
    executeNamed conn updateUserQuery $
           [ ":username"      := username
           , ":shell"         := shell
           , ":homeDirectory" := homeDirectory
           , ":realName"      := realName
           , ":phone"         := phone
           , ":oldUsername"   := oldUsername ]
