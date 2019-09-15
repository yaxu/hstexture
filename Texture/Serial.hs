module Texture.Serial where

import qualified System.Hardware.Serialport as SP
import qualified Data.ByteString.Char8 as B
import Texture.Weave (Weave, defaultWeave, to2d, wBits)


heddleMap = [0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15]

openSerial :: String -> IO ([Bool] -> IO ())
openSerial dev = do port <- SP.openSerial dev (SP.defaultSerialSettings
                                               {SP.commSpeed = SP.CS9600}
                                              )
                    return $ _send port

_send :: SP.SerialPort -> [Bool] -> IO ()
_send port bits = 
  do SP.send port $ B.pack $ str ++ "x"
     return ()
       where
         str = map (toChar . (bits !!)) heddleMap
         toChar True = '1'
         toChar False = '0'
  

data Loom = Loom {lWeave :: Weave,
                  lRow :: Int,
                  lSend :: ([Bool] -> IO ())
                 }

dev = "/dev/serial/by-id/usb-Arduino__www.arduino.cc__0043_557363239393515181E2-if00"

loom :: IO (Loom)
loom = do -- send <- openSerial dev
          return $ Loom {lWeave = defaultWeave,
                         lRow = 0,
                         lSend = putStrLn . show -- send
                        }


rows :: Loom -> [[Bool]]
rows = cycle . to2d . wBits . lWeave

sendRow :: Loom -> IO ()
sendRow loom =
  do let r = (rows loom) !! lRow loom
     putStrLn $ show $ take 16 r
     lSend loom r
     return ()

