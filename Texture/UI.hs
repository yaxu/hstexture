module Texture.UI where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.Array.IArray

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Graphics.UI.SDL.TTF.Types
import Data.Maybe (listToMaybe, fromMaybe, fromJust, isJust)
import GHC.Int (Int16)
import qualified Texture.Types as T

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

data Word = Word {ident :: Int,
                  token :: String,
                  location :: (Float, Float),
                  size :: (Float, Float),
                  mousePos :: Maybe (Float, Float)
                 }
           deriving Show

data Scene = Scene {source :: [Word],
                    parsed :: [T.Datum]
                   }
           deriving Show

parseScene :: [Word] -> Scene
parseScene ws = Scene ws (T.build $ map wordToDatum ws)

wordToDatum :: Word -> T.Datum
wordToDatum w = T.Datum {T.ident = ident w,
                         T.token = token w,
                         T.location = location w,
                         T.parentId = Nothing,
                         T.childId = Nothing,
                         T.sig  = s,
                         T.applied_as = s
                        }
  where s = T.stringToSig (token w)

toScreen :: (Float, Float) -> (Int, Int)
toScreen (x, y) = (floor (x * (fromIntegral screenWidth)),
                   floor (y * (fromIntegral screenHeight))
                  )

toScreen16 :: (Float, Float) -> (Int16, Int16)
toScreen16 (x, y) = (fromIntegral $ floor (x * (fromIntegral screenWidth)),
                     fromIntegral $ floor (y * (fromIntegral screenHeight))
                    )

fromScreen :: (Int, Int) -> (Float, Float)
fromScreen (x, y) = ((fromIntegral x) / (fromIntegral screenWidth),
                     (fromIntegral y) / (fromIntegral screenHeight)
                    )

withSource :: Scene -> ([Word] -> [Word]) -> Scene
withSource s f = s {source = f $ source s}

clearMouseOffset :: [Word] -> [Word]
clearMouseOffset = map (\instruction -> instruction {mousePos = Nothing})

setMouseOffset :: [Word] -> (Float, Float) -> [Word]
setMouseOffset ws (x,y) = 
  fromMaybe ws $ do i <- instructionAt ws (x,y)
                    let x' = x - (fst $ location i)
                        y' = y - (snd $ location i)
                    return $ setWord ws $ i {mousePos = Just (x',y')}

moveWord :: [Word] -> (Float, Float) -> [Word]
moveWord ws (x,y) =  
  fromMaybe ws $ do i <- moving ws
                    (xOffset, yOffset) <- mousePos i
                    let x' = x - xOffset
                        y' = y - yOffset
                    return $ setWord ws $ i {location = (x',y')}

inWord :: (Float, Float) -> Word -> Bool
inWord (px,py) Word {size = (w,h), location = (x,y)} =
  and [px >= x, py >= y, px < x+w, py < y+h]

instructionAt :: [Word] -> (Float, Float) -> Maybe Word
instructionAt ws location = listToMaybe $ filter (inWord location) ws

moving :: [Word] -> Maybe Word
moving = listToMaybe . (filter (isJust . mousePos))

setWord :: [Word] -> Word -> [Word]
setWord [] _ = []
setWord (x:xs) i | ident x == ident i = (i:xs)
                 | otherwise = x:(setWord xs i)

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y = (x' > rx) && (x' < rx + rw) && (y' > ry) && (y' < ry + rh)
 where (x', y') = (fromIntegral x, fromIntegral y)

handleEvent :: Scene -> Event -> Scene
handleEvent scene (MouseMotion x y _ _) = 
  parseScene $ source $ withSource scene (\ws -> moveWord ws (fromScreen (fromIntegral x, fromIntegral y)))

handleEvent scene (MouseButtonDown x y ButtonLeft) = 
  withSource scene (\ws -> setMouseOffset ws (fromScreen (fromIntegral x, fromIntegral y)))
	
handleEvent scene (MouseButtonUp x y ButtonLeft) = 
  withSource scene clearMouseOffset

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

drawScene :: Scene -> Font -> Surface -> IO ()
drawScene scene font screen = 
  do mapM_ (\i -> 
             do let (x, y) = toScreen $ location i
                    (w, h) = toScreen $ size i
                fillRect screen (Just $ Rect x y w h) (Pixel 0x00333333)
                message <- renderTextSolid font (token i) textColor
                applySurface 
                  (floor $ (fromIntegral screenWidth) * (fst $ location i)) 
                  (floor $ (fromIntegral screenHeight) * (snd $ location i)) 
                  message screen Nothing
           ) (source scene)
     mapM_ (\d ->
             do let (x1, y1) = T.location d
                    (x2, y2) = T.location (fromJust $ T.child d (parsed scene))
                (thickLine 0.01 x1 y1 x2 y2) screen lineColor
           )
       (filter T.hasChild $ parsed scene)
  where textColor = Color 255 255 255
        lineColor = Pixel 0x777777

thickLine :: Float -> Float -> Float -> Float -> Float -> (Surface -> Pixel -> IO Bool)
thickLine thickness x1 y1 x2 y2 = \s p -> do SDLP.filledPolygon s (map toScreen16 coords) p
                                             SDLP.aaPolygon s (map toScreen16 coords) p
  where x = x2 - x1
        y = y2 - y1
        l = sqrt $ x*x+y*y
        ox = (thickness * (y2-y1) / l)/2
        oy = (thickness * (x1-x2) / l)/2
        coords = [(x1 + ox, y1 + oy),
                  (x2 + ox, y2 + oy),
                  (x2 - ox, y2 - oy),
                  (x1 - ox, y1 - oy)
                 ]


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
        drawScene scene font screen
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

newWord :: Int -> String -> (Float, Float) -> Font -> IO (Word)
newWord ident text location font =
  do (w, h) <- textSize text font
     return $ Word ident text location (w, h) Nothing

main = withInit [InitEverything] $ 
       do result <- TTFG.init
          if not result
             then putStrLn "Failed to init ttf"
            else do env <- initEnv
                    a <- newWord 0 "+" (0.3, 0.3) (font env)
                    b <- newWord 1 "1" (0.4, 0.4) (font env)
                    c <- newWord 2 "2" (0.5, 0.5) (font env)
                    let scene = parseScene [a,b,c]
                    putStrLn $ show scene
                    runLoop env scene
