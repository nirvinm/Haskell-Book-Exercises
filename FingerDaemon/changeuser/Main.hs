{-# LANGUAGE OverloadedStrings #-}

-- 2. Write an executable separate of fingerd and debug which allows
-- you to add new users to the database.

module Main where

import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import System.Exit
import Text.RawString.QQ

import FingerDB


validate :: Text -> IO Text
validate s =
    case Data.Text.strip s of
        "" -> die "Empty text is not allowed. Terminating.."
        x  -> return x

prompt :: String -> IO Text
prompt s = do
    putStrLn s
    value <- getLine
    validate $ pack value


readUsrRow :: IO UserRow
readUsrRow = do
    username <- prompt "Username? "
    shell    <- prompt "Shell? "
    homdir   <- prompt "Home directory? "
    realname <- prompt "Real name? "
    phone    <- prompt "Phone? "

    return (Null, username, shell,
            homdir, realname, phone)


newUser :: Connection -> IO ()
newUser conn = do
    user <- readUsrRow
    addUser conn user


modifyUser :: Connection -> IO ()
modifyUser conn = do
    oldUsername <- prompt "Existing username? "
    oldUser <- getUser conn oldUsername
    case oldUser of
        Nothing -> die "No such user exists. Terminating.."
        Just _  -> putStrLn "Now enter new values.."
    newUser <- readUsrRow
    updateUser conn oldUsername newUser


data Command = AddUser | ModifyUser

getCommand :: IO Command
getCommand = do
    putStrLn "[1] Add new user"
    putStrLn "[2] Modify existing user"
    c <- getLine
    case c of
        "1" -> return AddUser
        "2" -> return ModifyUser
        _   -> die "Invalid choice. Terminating.."

main :: IO ()
main = do
    conn <- open "finger.db"
    cmd <- getCommand
    case cmd of
        AddUser    -> newUser conn
        ModifyUser -> modifyUser conn
    close conn
    putStrLn "Operation success."
