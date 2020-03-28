#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0

{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import qualified Options.Applicative as O
import qualified Data.Foldable as F
import qualified Control.Applicative as CA


defaultConfFile :: String
defaultConfFile = "/usr/local/etc/zetaback.conf"

data Command = 
    Version
  | Listing
  | Backup FilePath (Maybe String) 
  | Archive FilePath (Maybe String) 
  | Recover FilePath (Maybe String) 
  deriving (Show)   

parserInfo' :: O.ParserInfo Command  
parserInfo' = info' parser' "zetabackhaskell - your friendy zetaback clone written in Haskell"
  where
    parser' :: O.Parser Command 
    parser' = (O.subparser . F.foldMap command') 
        [ ("version", "show version", pure Version)
        , ("listing", "show available backups", pure Listing)
        , ("backup", "perform backup", backupP)
        , ("archive", "archive a backup set", archiveP) 
         , ("recover", "recover from backup", recoverP) 
        ] 

    backupP = Backup <$> confOpt <*> zfsOpt 
    archiveP = Archive <$> confOpt <*> zfsOpt  
    recoverP = Recover <$> confOpt <*> zfsOpt  

-- https://stackoverflow.com/questions/32422339/how-to-parse-an-optional-flag-as-a-maybe-value

    zfsOpt = (CA.optional . O.strOption)
        (mconcat 
            [ O.long "zfs"
            , O.short 'z'
            , O.metavar "ZFS"
            , O.help "ZFS pool to work with" ])

    confOpt = O.strOption
        (mconcat 
            [ O.help "Use the specified file as the configuration file."
            , O.value defaultConfFile
            , O.showDefault
            , O.long "conf"
            , O.short 'c' 
            , O.metavar "CONF" ])

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info 
        (O.helper <*> p) 
        (O.fullDesc <> O.progDesc desc)
            
    command' (cmdName,desc,parser) = 
        O.command cmdName (info' parser desc)


showHelpOnErrorExecParser :: O.ParserInfo a -> IO a
showHelpOnErrorExecParser = O.customExecParser (O.prefs O.showHelpOnEmpty)

main :: IO ()
main = do
    cmd <- showHelpOnErrorExecParser parserInfo'
    case cmd of
        Version -> putStrLn "zetabackhaskell 0.01 - inspired by zetaback.hs: 1.0.6"
        Backup conffile (Just zfs) -> putStrLn ("backup up pool " ++ zfs ++ " using config " ++ conffile)
        Backup conffile _ -> putStrLn ("backup up all pools using config " ++ conffile  )
        Recover conffile (Just zfs) -> putStrLn ("recovering pool " ++ zfs ++ " using config " ++ conffile)
        Archive conffile (Just zfs)  -> putStrLn ("archive  ..." ++ conffile ++ " " ++ zfs )
        _ -> putStrLn "should be doing something"
    return ()




-- https://stackoverflow.com/questions/57168091/parsing-user-options-into-custom-data-types-with-optparse-applicative
-- http://robb.re/notes/optparse-applicative.html
-- https://medium.com/@danidiaz/subcommands-with-optparse-applicative-1234549b21c6
-- https://github.com/danidiaz/vdpq/blob/master/executable/Main.hs

-- thoughtbot.hs start http://www.google.de 123 --app COMPILE-APP
-- ./thoughtbot.hs -a 123 start url version
-- stack --resolver=lts-8.0 ghci danidiaz_vdpq_executable_Main.hs


{-  

Usage:
      zetaback.hs version

      zetaback.hs [-l|-s|-sx|-sv|-svv] [--files] [-c conf] [-d] [-h host] [-z zfs]

      zetaback.hs -a [-c conf] [-d] [-h host] [-z zfs]

      zetaback.hs -b [-ff] [-fi] [-x] [-c conf] [-d] [-n] [-h host] [-z zfs]

      zetaback.hs -x [-b] [-c conf] [-d] [-n] [-h host] [-z zfs]

      zetaback.hs -r [-c conf] [-d] [-n] [-h host] [-z zfs] [-t timestamp]
                  [-rhost host] [-rzfs fs]

-}