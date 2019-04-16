module CmdArgsParser where

import Options.Applicative
import Data.Semigroup ((<>))


data Command = Encrypt | Decrypt

data Args =
    Args { command :: Command
         , key :: String
         , timeout :: Int
         }

commandArg :: Parser Command
commandArg = 
        ( flag' Encrypt ( short 'e'
                       <> long "encrypt"
                       <> help "Encrypt" ) )
    <|> ( flag' Decrypt ( short 'd'
                       <> long "decrypt"
                       <> help "Decrypt" ) )

keyArg :: Parser String
keyArg = strOption
     ( long "key"
    <> short 'k'
    <> help "Ecryption/Decryption Key" )

timeoutArg :: Parser Int
timeoutArg  = option auto
     ( long "timeout"
    <> short 't'
    <> showDefault
    <> value (-1)
    <> help "Timeout in milliseconds" )

programArgs :: Parser Args
programArgs = Args <$> commandArg <*> keyArg <*> timeoutArg

programInfo :: ParserInfo Args
programInfo = info (programArgs <**> helper)
     ( fullDesc
    <> progDesc "Encrypt or decrypt text with Vigenere Cipher." )
