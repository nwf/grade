{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import           Data.Monoid
import           Options.Applicative

import           Grade.Methods

defineArg = argument str (metavar "DEFINES" <> help "The defines file")
infoh p i = info (p <**> helper) i

cmds :: Parser (IO ())
cmds = subparser
  (  commandGroup "Before"
  <> (command "make-skeleton" $
       infoh (doMakeSkeleton <$> defineArg)
             (progDesc "Make grader skeleton from defines file"))
  )
  <|> subparser
  (  hidden
  <> commandGroup "After"
  <> (command "grade-one" $
       infoh (doGradeOne
               <$> defineArg
               <*> argument str (metavar "DATA" <> value "/dev/fd/0" <> help "Data file (defaults to stdin)"))
             (progDesc "Grade one student file"))
  <> (command "grade-dir" $
       infoh ((\v d o i -> doGradeDir v d i o)
               <$> option auto (long "verbose" <> metavar "V" <> value 0 <> help "Be chatty (integer)")
               <*> defineArg
               <*> argument str (metavar "OUT-DIR" <> help "Destination directory for output")
               <*> argument str (metavar "IN-DIR" <> value "." <> help "Input directory of grade files (default to $PWD)"))
             (progDesc "Grade a directory of student files"))
  )

main :: IO ()
main = join $ execParser
     $ infoh cmds
       (fullDesc
       <> progDesc "GRADE Reporting And Definition Environment"
       )
