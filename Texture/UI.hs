module Texture.UI where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
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
import Data.List (intercalate)

import Texture.Utils
import Texture.Interp (start)
import Stream (OscPattern)
import Dirt

screenWidth  = 1024
screenHeight = 768
screenBpp    = 32

xDivider = 0.85

data WordStatus = Active
                | Tentative
                | MenuItem
                deriving (Show, Eq)

data Word = Word {ident :: Int,
                  token :: String,
                  location :: (Float, Float),
                  size :: (Float, Float),
                  mousePos :: Maybe (Float, Float),
                  status :: WordStatus
                 }
           deriving Show

instance Eq Word where
  (Word {ident = a}) == (Word {ident = b}) = a == b

data Scene = Scene {source :: [Word],
                    parsed :: [T.Datum]
                   }
           deriving Show

parseScene :: [Word] -> Scene
parseScene ws = Scene ws (T.build $ map wordToDatum (filterActive ws))

nextIdent :: [Word] -> Int
nextIdent ws = head $ filter (\i -> null $ filter ((== i) . ident) ws) [0 ..]

wordToDatum :: Word -> T.Datum
wordToDatum w = T.Datum {T.ident = ident w,
                         T.token = token w,
                         T.location = location w,
                         T.parentId = Nothing,
                         T.childIds = [],
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

withActive :: Scene -> ([Word] -> [Word]) -> Scene
withActive s f = withSource s (f . filterActive)

filterActive :: [Word] -> [Word]
filterActive = (filter ((== Active) . status))

clearMouseOffset :: [Word] -> [Word]
clearMouseOffset = (map clearTentative) . (map f) . (filter filtF)
  where f i = i {mousePos = Nothing}
        filtF i@(Word {status = Tentative, location = (x,_), size = (w,_)}) 
          = (x + w) <= xDivider
        filtF _ = True
        clearTentative i@(Word {status = Tentative}) = i {status = Active}
        clearTentative i = i

setMouseOffset :: [Word] -> (Float, Float) -> [Word]
setMouseOffset ws (x,y) = 
  fromMaybe ws $ do i <- instructionAt ws (x,y)
                    let x' = x - (fst $ location i)
                        y' = y - (snd $ location i)
                    return $ setWord ws $ i {mousePos = Just (x',y')}

moveWord :: Scene -> (Float, Float) -> AppEnv Scene
moveWord s (x,y) | w == Nothing = return s
                 | otherwise = do ws <- moveWord' (source s) (x,y) (fromJust w)
                                  let s = parseScene ws
                                      code = T.walkTreesWhere (T.isOscPattern . T.applied_as) (parsed s)
                                  if (null code) then 
                                    liftIO $ putStrLn "empty"
                                    else
                                    do 
                                      let code' = "stack [" ++ (intercalate ", " code) ++ "]"
                                      i <- input `liftM` ask
                                      liftIO $ putStrLn $ "sending '" ++ code' ++ "'"
                                      liftIO $ putMVar i code'
                                  return $ s
  where w = moving $ source s

moveWord' :: [Word] -> (Float, Float) -> Word -> AppEnv [Word]
moveWord' ws loc wd@(Word {status = MenuItem}) = moveWord' ws' loc newWord
  where newWord = wd {status = Tentative, ident = nextIdent ws}
        ws' = newWord:(clearMouseOffset ws)

moveWord' ws (x,y) wd =
  return $ fromMaybe ws $ 
       do (xOffset, yOffset) <- mousePos wd
          let (w,h) = size wd
              x' | status wd == Tentative = (x-xOffset)
                 | otherwise = max 0 $ min (xDivider - w) (x - xOffset)
              y' = max 0 $ min (1 - h) $ y - yOffset
          return $ setWord ws $ wd {location = (x',y')}

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

handleEvent :: Scene -> Event -> AppEnv (Scene)
handleEvent s (MouseMotion x y _ _) = 
  do s' <- moveWord s (fromScreen (fromIntegral x, fromIntegral y))
     return $ parseScene $ source s'


--return $ parseScene $ source $ withSource scene (\ws -> moveWord ws (fromScreen (fromIntegral x, fromIntegral y)))

handleEvent scene (MouseButtonDown x y ButtonLeft) = 
  return $ withSource scene (\ws -> setMouseOffset ws (fromScreen (fromIntegral x, fromIntegral y)))
	
handleEvent scene (MouseButtonUp x y ButtonLeft) = 
  return $ parseScene $ source $ withSource scene clearMouseOffset

handleEvent scene _ = return $ scene

data AppConfig = AppConfig {
  screen       :: Surface,
  font         :: Font,
  input        :: MVar String,
  output       :: MVar OscPattern
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
    o <- dirtstart "texture"
    i <- start o
    return $ AppConfig screen font i o

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
                mapM_ (\childId -> do 
                          let (x2, y2) = T.location (T.datumByIdent childId (parsed scene))
                          (thickLine 0.01 x1 y1 x2 y2) screen lineColor
                      
                      ) (T.childIds d)
           )
       (filter T.hasChild $ parsed scene)
  where textColor = Color 255 255 255
        lineColor = Pixel 0x777777

thickLine :: Float -> Float -> Float -> Float -> Float -> (Surface -> Pixel -> IO Bool)
thickLine thickness x1 y1 x2 y2 = \s p -> do SDLP.filledPolygon s (map toScreen16 coords) p
                                             SDLP.aaPolygon s (map toScreen16 coords) p
                                             SDLP.filledPolygon s (map toScreen16 arrowCoords) p
                                             SDLP.aaPolygon s (map toScreen16 arrowCoords) p
  where x = x2 - x1
        y = y2 - y1
        headx = (x/l) / 20
        heady = (y/l) / 20
        l = sqrt $ x*x+y*y
        ox = (thickness * (y2-y1) / l)/2
        oy = (thickness * (x1-x2) / l)/2
        coords = [(x1 + ox, y1 + oy),
                  ((x2 + ox) - headx, (y2 + oy) - heady),
                  ((x2 - ox) - headx, (y2 - oy) - heady),
                  (x1 - ox, y1 - oy)
                 ]
        arrowCoords = [((x2 + ox*2) - headx, (y2 + oy*2) - heady),
                       ((x2 - ox*2) - headx, (y2 - oy*2) - heady),
                       (x2, y2)
                      ]


loop :: AppEnv ()
loop = do
    quit <- whileEvents $ act
    screen <- screen `liftM` ask
    font <- font `liftM` ask
    scene <- get
    -- a local lambda so we don't have use liftIO for all the SDL actions used which are in IO.
    liftIO $ do
        bgColor  <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00  
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect bgColor
        SDLP.aaLine screen (floor $ xDivider * (fromIntegral screenWidth)) 0 (floor $ xDivider * (fromIntegral screenWidth)) (fromIntegral screenHeight) (Pixel 0x00ffffff)
        drawScene scene font screen
        Graphics.UI.SDL.flip screen
    unless quit loop
      where act e = do scene <- get 
                       scene' <- handleEvent scene e
                       put $ scene'

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

newWord :: Int -> String -> (Float, Float) -> Font -> WordStatus -> IO (Word)
newWord ident text location font status = setSize wd font
  where wd = Word ident text location undefined Nothing status

setSize :: Word -> Font -> IO Word
setSize wd font = do sz <- textSize (token wd) font
                     return $ wd {size = sz}

wordMenu :: Font -> [String] -> IO ([Word])
wordMenu font ws = mapM addWord (enumerate ws)
  where addWord (n, w) = 
          newWord n w (xDivider + 0.01, (fromIntegral n) * 0.04) font MenuItem

things = (map fst T.functions) ++ ["1", "2", "3", "2.5", "6.2"]

run = withInit [InitEverything] $ 
      do result <- TTFG.init
         if not result
           then putStrLn "Failed to init ttf"
           else do env <- initEnv
                   ws <- wordMenu (font env) things
                   let scene = parseScene ws
                   --putStrLn $ show scene
                   runLoop env scene
