{-# LANGUAGE OverloadedStrings #-}

module CommandParser where

import Control.Applicative
import Data.Text
import Text.Trifecta
import Database.SQLite.Simple.Types

import FingerDB


parseArgument :: Parser Text
parseArgument = do
    optional spaces
    val <- stringLiteral
    optional spaces
    return $ pack val

parseUserArg :: Parser UserRow
parseUserArg = do
    username <- parseArgument <?> "Username"
    shell    <- parseArgument <?> "Shell"
    homeDir  <- parseArgument <?> "Home Directory"
    realName <- parseArgument <?> "Real Name"
    phone    <- parseArgument <?> "Phone number"
    
    return $ (Null, username, shell,
              homeDir, realName, phone)
