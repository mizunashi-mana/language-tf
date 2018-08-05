{-# LANGUAGE RecordWildCards #-}

module Main where

import           Language.TF.Parser
import           Language.TF.Printer
import           System.IO           (BufferMode (LineBuffering), hPrint,
                                      hPutStrLn, hSetBuffering, stderr, stdin,
                                      stdout)
import           Text.Trifecta


main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  pstr <- getContents
  case parseString hoistedAstParser mempty pstr of
    Success ast -> do
      putStrLn "Success to parse:"
      print $ printAst ast
    Failure ErrInfo{..} -> do
      hPutStrLn stderr "Failed to parse:"
      hPrint stderr _errDoc
