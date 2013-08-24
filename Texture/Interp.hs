{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Texture.Interp where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Language.Haskell.Interpreter
import Sound.OpenSoundControl.Type

import Stream
import Pattern
import qualified Texture.Types as T
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Hashable
import Data.Bits

import Data.Typeable
deriving instance Typeable Param
deriving instance Typeable1 Pattern
deriving instance Typeable Sound.OpenSoundControl.Type.Datum

data Job = OscJob String
         | ColourJob T.Type String

start :: MVar OscPattern -> MVar (Pattern (Colour Double)) -> IO (MVar Job)
start oscOut colourOut = do input <- newEmptyMVar
                            forkIO $ loop input 
                            return input
  where loop input = 
          do r <- runInterpreter $ runI input oscOut colourOut
             case r of
               Left err -> printInterpreterError err
               Right () -> putStrLn "Eh?"
             loop input

libs = [("Prelude", Nothing), 
        ("Stream", Nothing), ("Dirt", Nothing), ("Pattern", Nothing),
        ("Data.Map", Nothing), ("Sound.OpenSoundControl", Nothing),
        ("Parse", Nothing),
        ("Control.Applicative", Nothing)
       ]

runI :: MVar Job -> MVar OscPattern -> MVar (Pattern (Colour Double)) -> Interpreter ()
runI input oscOut colourOut =
    do
      --loadModules ["Stream.hs"]
      --setTopLevelModules ["SomeModule"]
      
      setImportsQ libs
      loop
  where loop = do 
          thing <- liftIO (takeMVar input)
          --say =<< Language.Haskell.Interpreter.typeOf expr
          doJob thing
          loop
        doJob (OscJob code) = do p <- interpret code (as :: OscPattern)
                                 say (show p)
                                 liftIO $ swapMVar oscOut p
                                 return ()
        doJob (ColourJob t code) = do p <- interpretPat t code
                                      say $ "interpreting: " ++ code ++ " type " ++ (show t)
                                      say $ " as: " ++ (show p)
                                      liftIO $ putMVar colourOut p
                                      return ()


          
{-
run2 :: String -> Interpreter (Pattern (Colour Double))
run2 input output =
    do
      --loadModules ["Stream.hs"]
      --setTopLevelModules ["SomeModule"]
      
      setImportsQ libs
      loop
  where loop = do 
          expr <- liftIO (takeMVar input)
          say =<< Language.Haskell.Interpreter.typeOf expr

          p <- interpret expr (as :: OscPattern)
          say (show p)
          liftIO $ swapMVar output p
          loop
-}

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Oops. " ++ (show e)

interpretPat :: T.Type -> String -> Interpreter (Pattern (Colour Double))

interpretPat T.String code = 
  do setImportsQ libs
     p <- interpret code (as :: Pattern String)
     return $ fmap stringToColour p
interpretPat _ _ = return silence

stringToColour :: String -> Colour Double
stringToColour s = sRGB (r/256) (g/256) (b/256)
  where i = (hash s) `mod` 16777216
        r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16;
        g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8;
        b = fromIntegral $ (i .&. 0x0000FF);
