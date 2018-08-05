{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Simple
import Distribution.Extra.Doctest ( generateBuildModule )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = buildHookScript
  }
  where
    buildHookScript pkg lbi hooks flags = do
      generateBuildModule "doctest" flags pkg lbi -- generate Build_doctests
      buildHook simpleUserHooks pkg lbi hooks flags

#else

{-# WARNING [ "You are configuring this package without cabal-doctest installed."
            , "The doctests test-suite will not work as a result."
            , "To fix this, install cabal-doctest before configuring."
            ] #-}

main :: IO ()
main = defaultMain

#endif
