module Texture.Serial where

import qualified System.Hardware.Serialport as SP
import qualified Data.ByteString.Char8 as B
import Texture.Weave (Weave, defaultWeave, to2d, wBits)


-- heddleMap = [0,2,4,6,8,10,12,14,1,3,5,7,9,11,13,15]
heddleMap = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

openSerial :: String -> IO ([Bool] -> IO ())
openSerial dev = do port <- SP.openSerial dev (SP.defaultSerialSettings
                                               {SP.commSpeed = SP.CS9600}
                                              )
                    return $ _send port

_send :: SP.SerialPort -> [Bool] -> IO ()
_send port bits = 
  do putStrLn "foo"
     SP.send port $ B.pack $ str ++ "x"
     putStrLn "bar"
     return ()
       where
         str = map (toChar . ((bits ++ repeat False) !!)) heddleMap
         toChar True = '1'
         toChar False = '0'
  

data Loom = Loom {lWeave :: Weave,
                  lRow :: Int,
                  lSend :: ([Bool] -> IO ())
                 }

-- dev = "/dev/serial/by-id/usb-Arduino__www.arduino.cc__0043_557363239393515181E2-if00"
dev = "/dev/serial/by-id/usb-Arduino__www.arduino.cc__0043_557363239393515181E2-if00"

loom :: IO (Loom)
loom = do -- send <- openSerial dev
          return $ Loom {lWeave = defaultWeave,
                         lRow = 0
                         -- ,
                         -- lSend = send
                        }


rows :: Loom -> [[Bool]]
rows = safeCycle . to2d . wBits . lWeave
  where safeCycle [] = []
        safeCycle x = cycle x

sendRow :: Rational -> Loom -> IO ()
sendRow cps loom =
  do putStrLn "Sending row"
     let r = ((rows loom) ++ (repeat [])) !! lRow loom
     putStrLn $ show $ take 16 $ r ++ repeat False
     --lSend loom r
     putStrLn "Sent."
     return ()

