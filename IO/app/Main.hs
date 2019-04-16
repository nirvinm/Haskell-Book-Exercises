module Main where

import Options.Applicative

import System.Environment
import System.Exit
import System.IO

import CmdArgsParser
import VigenereCipher


encryptStdin :: Key -> IO String
encryptStdin k = (encrypt k) <$> hGetContents stdin

decryptStdin :: Key -> IO String
decryptStdin k = (decrypt k) <$> hGetContents stdin


main :: IO ()
main = do
    args <- execParser programInfo
    inputAvailable <- hWaitForInput stdin $ timeout args
    output <-
        case (inputAvailable, args) of
            (True, Args Encrypt key _) -> encryptStdin key
            (True, Args Decrypt key _) -> decryptStdin key
            (False, _) -> die "Timed out. Quitting.."
    hPrint stdout output
