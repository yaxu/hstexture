module UI where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.Word
import Data.Array.IArray

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Graphics.UI.SDL.TTF.Types
import Data.Maybe (listToMaybe, fromMaybe, isJust)

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

data Instruction = Instruction {identifier :: Int,
                                label :: String,
                                location :: (Float, Float),
                                size :: (Float, Float),
                                mousePos :: Maybe (Float, Float)
                               }

type Scene = [Instruction]

toScreen :: (Float, Float) -> (Int, Int)
toScreen (x, y) = (floor (x * (fromIntegral screenWidth)),
                   floor (y * (fromIntegral screenHeight))
                  )

fromScreen :: (Int, Int) -> (Float, Float)
fromScreen (x, y) = ((fromIntegral x) / (fromIntegral screenWidth),
                     (fromIntegral y) / (fromIntegral screenHeight)
                    )

clearMouseOffset :: Scene -> Scene
clearMouseOffset = map (\instruction -> instruction {mousePos = Nothing})

setMouseOffset :: Scene -> (Float, Float) -> Scene
setMouseOffset scene (x,y) = 
  fromMaybe scene $ do i <- instructionAt scene (x,y)
                       let x' = x - (fst $ location i)
                           y' = y - (snd $ location i)
                       return $ setInstruction scene $ i {mousePos = Just (x',y')}

moveInstruction :: Scene -> (Float, Float) -> Scene
moveInstruction scene (x,y) =  
  fromMaybe scene $ do i <- moving scene
                       (xOffset, yOffset) <- mousePos i
                       let x' = x - xOffset
                           y' = y - yOffset
                       return $ setInstruction scene $ i {location = (x',y')}

inInstruction :: (Float, Float) -> Instruction -> Bool
inInstruction (px,py) Instruction {size = (w,h), location = (x,y)} =
  and [px >= x, py >= y, px < x+w, py < y+h]

instructionAt :: Scene -> (Float, Float) -> Maybe Instruction
instructionAt scene location = listToMaybe $ filter (inInstruction location) scene

moving :: Scene -> Maybe Instruction
moving scene = listToMaybe $ filter (isJust . mousePos) scene

setInstruction :: Scene -> Instruction -> Scene
setInstruction [] _ = []
setInstruction (x:xs) i | identifier x == identifier i = (i:xs)
                        | otherwise = x:(setInstruction xs i)

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y = (x' > rx) && (x' < rx + rw) && (y' > ry) && (y' < ry + rh)
 where (x', y') = (fromIntegral x, fromIntegral y)

handleEvent :: Scene -> Event -> Scene
handleEvent scene (MouseMotion x y _ _) = moveInstruction scene (fromScreen (fromIntegral x, fromIntegral y)) 

handleEvent scene (MouseButtonDown x y ButtonLeft) = 
  setMouseOffset scene (fromScreen (fromIntegral x, fromIntegral y))
	
handleEvent scene (MouseButtonUp x y ButtonLeft) = clearMouseOffset $ scene

handleEvent scene _ = scene

data AppConfig = AppConfig {
    screen       :: Surface,
    font         :: Font
}

type AppState = StateT Scene IO
type AppEnv = ReaderT AppConfig AppState

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

initEnv :: IO AppConfig
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    font <- openFont "inconsolata.ttf" 28
    setCaption "Texture" []
    return $ AppConfig screen font

showScene :: Scene -> Font -> Surface -> IO ()
showScene scene font screen = 
  mapM_ (\i -> 
          do let (x, y) = toScreen $ location i
                 (w, h) = toScreen $ size i
             fillRect screen (Just $ Rect x y w h) (Pixel 0x00333333)
             message <- renderTextSolid font (label i) textColor
             applySurface 
               (floor $ (fromIntegral screenWidth) * (fst $ location i)) 
               (floor $ (fromIntegral screenHeight) * (snd $ location i)) 
               message screen Nothing
        ) scene
  where textColor = Color 255 255 255

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ modify . (Prelude.flip handleEvent)
    
    screen <- screen `liftM` ask
    font <- font `liftM` ask
    scene <- get
    
    -- a local lambda so we don't have use liftIO for all the SDL actions used which are in IO.
    liftIO $ do
        bgColor  <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00  
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect bgColor
        showScene scene font screen
        Graphics.UI.SDL.flip screen
    
    unless quit loop

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> Scene -> IO ()
runLoop = evalStateT . runReaderT loop

textSize :: String -> Font -> IO ((Float,Float))
textSize text font = 
  do message <- renderTextSolid font text (Color 0 0 0)
     return (fromScreen (surfaceGetWidth message, surfaceGetHeight message))

newInstruction :: Int -> String -> (Float, Float) -> Font -> IO (Instruction)
newInstruction identifier text location font =
  do (w, h) <- textSize text font
     return $ Instruction identifier text location (w, h) Nothing

main = withInit [InitEverything] $ 
       do result <- TTFG.init
          if not result
             then putStrLn "Failed to init ttf"
            else do env <- initEnv
                    a <- newInstruction 0 "+" (0.3, 0.3) (font env)
                    b <- newInstruction 1 "1" (0.4, 0.4) (font env)
                    c <- newInstruction 2 "2" (0.5, 0.5) (font env)
                    runLoop env [a,b,c]
