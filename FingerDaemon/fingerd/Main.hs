{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Control.Concurrent

import Data.List (intersperse, isPrefixOf)

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.RawString.QQ

import Data.Typeable

import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import Network.Socket hiding (close, recv)
import Network.Socket.ByteString (recv, sendAll)

import Text.Trifecta

import CommandParser

----- ALL DB TYPES AND QUERIES MOVED TO src/FingerDB.hs -------
import FingerDB
---------------------------------------------------------------

toBS :: Show a => a -> ByteString
toBS = encodeUtf8 . T.pack . show


fromBS :: ByteString -> String
fromBS = T.unpack . decodeUtf8


returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn soc = do
    rows <- getAllUsers dbConn
    let usernames = map userName rows
        newlineSeparated =
            T.concat $
                intersperse "\n" usernames
    sendAll soc (encodeUtf8 newlineSeparated)


formatUser :: User -> ByteString
formatUser (User _ username shell
            homeDir realName _) =
    BS.concat
        ["Login: ", e username, "\n",
         "Name: ", e realName, "\n",
         "Directory: ", e homeDir, "\n",
         "Shell: ", e shell, "\n"]
    where e = encodeUtf8


returnUser :: Connection
           -> Socket
           -> Text
           -> IO ()
returnUser dbConn soc username = do
    maybeUser <-
        getUser dbConn (T.strip username)
    case maybeUser of
        Nothing -> do
            putStrLn
              ("Couldn't find matching user\
               \ for username: "
              ++ (show username))
        Just user ->
            sendAll soc (formatUser user)


handleQuery :: Connection
            -> Socket
            -> IO ()
handleQuery dbConn soc = do
    msg <- recv soc 1024
    case msg of
        "\r\n" -> returnUsers dbConn soc
        name   -> returnUser dbConn soc $
                    decodeUtf8 name


handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries dbConn sock = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery dbConn soc
    sClose soc


handleCommand :: Connection -> Socket -> IO ()
handleCommand conn soc = do
    msg <- fromBS <$> recv soc 1024
    putStrLn "Command received, processing\n"
    case msg of
        (':':'a':'d':'d':xs) ->
            let result = parseString parseUserArg mempty xs in
            case result of
                Success a -> do
                    addUser conn a
                    sendAll soc "User added."
                Failure e ->
                    sendAll soc (toBS e)
        otherwise ->
            sendAll soc "Error. Could not understand command.\n"


handleCommands :: Connection
               -> Socket
               -> IO ()
handleCommands conn sock = forever $ do
    (soc, _) <- accept sock
    handleCommand conn soc
    sClose soc


listenForQueries :: Connection -> IO ()
listenForQueries conn = withSocketsDo $ do
    addrinfos <-
        getAddrInfo
            (Just $ defaultHints {addrFlags = [AI_PASSIVE]})
            Nothing
            (Just "79")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr)
            Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1 -- only one connection open at a time
    putStrLn "Listening for Finger queries at port 79"
    handleQueries conn sock
    sClose sock


listenForCommands :: Connection -> IO ()
listenForCommands conn = withSocketsDo $ do
    addrinfos <-
        getAddrInfo
            (Just $ defaultHints {addrFlags = [AI_PASSIVE]})
            Nothing
            (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr)
            Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1 -- only one connection open at a time
    putStrLn "Listening for commands at port 3000"
    handleCommands conn sock
    sClose sock


main :: IO ()
main = do
    conn <- open "finger.db"
    forkIO $ listenForCommands conn -- listen for commands like adding new user in separte thread
    listenForQueries conn -- listen for all Finger queries in main thread
    SQLite.close conn

