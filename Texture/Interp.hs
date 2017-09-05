{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Texture.Interp where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Language.Haskell.Interpreter
import Sound.OSC.FD
import Sound.OSC.Datum

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import qualified Texture.Types as T
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Hashable
import Data.Bits
import Data.Maybe
import Sound.Tidal.Dirt
import Sound.Tidal.OscStream
import qualified Data.Map as Map
import Control.Applicative
import qualified Data.ByteString.Char8 as C

import Data.Typeable
deriving instance Typeable Param
-- deriving instance Typeable1 Pattern
deriving instance Typeable Sound.OSC.FD.Datum

data Job = OscJob String
         | ColourJob T.Type String

start :: (ParamPattern -> IO ()) -> MVar (Maybe (Pattern (Colour Double))) -> IO (MVar Job)
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
        ("Sound.Tidal.Context", Nothing),
        ("Data.Map", Nothing), ("Sound.OSC", Nothing),
        ("Control.Applicative", Nothing)
       ]

runI :: MVar Job -> (ParamPattern -> IO ()) -> MVar (Maybe (Pattern (Colour Double))) -> Interpreter ()
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
        doJob (OscJob code) = do -- say ("interpreting: " ++ code)
                                 p <- interpret code (as :: (ParamPattern))
                                 liftIO $ oscOut p
                                 return ()
        doJob (ColourJob t code) = do -- say $ "interpreting: " ++ code ++ " type " ++ (show t)
                                      p <- interpretPat t code
                                      -- say $ " as colour pattern: " ++ (show p)
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

          p <- interpret expr (as :: (ParamPattern))
          say (show p)
          liftIO $ swapMVar output p
          loop
-}

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Oops. " ++ (show e)

interpretPat :: T.Type -> String -> Interpreter (Maybe (Pattern (Colour Double)))

interpretPat T.String code = 
  do p <- interpret code (as :: Pattern String)
     return $ Just $ fmap stringToColour p

interpretPat T.Float code =
  do p <- interpret code (as :: Pattern Double)
     return $ Just $ fmap (stringToColour . show) p

interpretPat T.Osc code =
  do p <- interpret code (as :: (ParamPattern))
     return $ Just $ stringToColour <$> unosc p

interpretPat _ _ = return Nothing

unosc :: ParamPattern -> Pattern String
unosc p = (\x -> fromMaybe (show x) (soundString x)) <$> p

soundString :: Map.Map Param (Value) -> Maybe [Char]
soundString o = do s <- Map.lookup (S "sound" Nothing) o
                   -- hack to turn ASCII into String
                   return $ tail $ init $ show s


{-
stringToColour :: String -> Colour Double
stringToColour s = sRGB (r/256) (g/256) (b/256)
  where i = (hash s) `mod` 16777216
        r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16;
        g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8;
        b = fromIntegral $ (i .&. 0x0000FF);
-}
