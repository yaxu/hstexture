{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Texture.Interp where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Language.Haskell.Interpreter
import Sound.OpenSoundControl.Type

import Stream
import Pattern

import Data.Typeable
deriving instance Typeable Param
deriving instance Typeable1 Pattern
deriving instance Typeable Sound.OpenSoundControl.Type.Datum

start :: MVar OscPattern -> IO (MVar String)
start output = do input <- newMVar "silence"
                  forkIO $ loop input output
                  return input
  where loop input output = 
          do r <- runInterpreter $ runI input output
             case r of
               Left err -> printInterpreterError err
               Right () -> putStrLn "Eh?"
             loop input output

runI :: MVar String -> MVar OscPattern -> Interpreter ()
runI input output =
    do
      --loadModules ["Stream.hs"]
      --setTopLevelModules ["SomeModule"]
      setImportsQ [("Prelude", Nothing), 
                   ("Stream", Nothing), ("Dirt", Nothing), ("Pattern", Nothing),
                   ("Data.Map", Nothing), ("Sound.OpenSoundControl", Nothing),
                   ("Parse", Nothing),
                   ("Control.Applicative", Nothing)
                  ]
      loop
  where loop = do 
          expr <- liftIO (takeMVar input)
          say =<< Language.Haskell.Interpreter.typeOf expr

          p <- interpret expr (as :: OscPattern)
          say (show p)
          liftIO $ swapMVar output p
          loop

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Oops. " ++ (show e)
