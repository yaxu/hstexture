{-# LANGUAGE FlexibleContexts #-}
module Texture.UI where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Array.IArray

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import qualified Graphics.UI.SDL.Framerate as FR
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Graphics.UI.SDL.TTF.Types
import Data.Maybe (listToMaybe, fromMaybe, fromJust, isJust, catMaybes)
import GHC.Int (Int16)
import qualified Texture.Types as T
import Data.List (intercalate, tails, nub, groupBy, sortBy)
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV (hsv)
import qualified GHC.Word 
import Data.Bits
import Data.Ratio
import Debug.Trace (trace)
import Data.Fixed (mod')
import Control.Concurrent
import System.Exit
import System.Random (newStdGen, randomRs)
import qualified Sound.OSC.FD as O
import Data.Function (on)

import Texture.Utils
import Texture.Interp (start, interpretPat, Job (WeaveJob, OscJob, ColourJob))
import Sound.Tidal.Stream
import Sound.Tidal.Carabiner
import Sound.Tidal.Pattern
import Sound.Tidal.Tempo
import Sound.Tidal.Config
import Sound.Tidal.Utils (enumerate)
import qualified Texture.Serial as L
import qualified Weave as W
  
--myIP = "192.168.0.2"
myIP = "127.0.0.1"

-- TODO - if width /= height, euclidean distances don't work..
screenWidth  = 1024
screenHeight = 720
screenBpp    = 32
linesz = 0.012

xDivider :: Float
xDivider = 0.75
-- xDivider = 1

data WordStatus = Active
                | ActiveSelected
                | Tentative
                | Typing
                | MenuItem
                deriving (Show, Eq)

data TWord = TWord {ident :: Int,
                    token :: String,
                    location :: (Float, Float),
                    size :: (Float, Float),
                    mousePos :: Maybe (Float, Float),
                    status :: WordStatus,
                    pat :: Maybe (Pattern (Colour Double)),
                    energy :: Float,
                    angle :: Float
                   }

instance Eq TWord where
  (TWord {ident = a}) == (TWord {ident = b}) = a == b

data Scene = Scene {source :: [TWord],
                    parsed :: [T.Datum],
                    mouseXY :: (Float, Float),
                    cursor :: (Float, Float)
                   }

getTyping :: [TWord] -> Maybe TWord
getTyping [] = Nothing
getTyping (x@(TWord {status = Typing}):_) = Just x
getTyping (_:xs) = getTyping xs

makeTyping :: [TWord] -> (Float, Float) -> TWord
makeTyping ws loc = TWord (nextIdent ws) "" loc (0,0) Nothing Typing Nothing 0 0

bottomRight :: TWord -> (Float, Float)
bottomRight (TWord {location = (x,y), size = (w,h)}) = (x+w, y+h)

bottomLeft :: TWord -> (Float, Float)
bottomLeft (TWord {location = (x,y), size = (w,h)}) = (x, y+h)

topRight :: TWord -> (Float, Float)
topRight (TWord {location = (x,y), size = (w,h)}) = (x+w, y)

parseScene :: Scene -> Scene
parseScene s = 
  s {parsed = (T.build $ map wordToDatum (filterActive $ source s))}

evalScene :: Scene -> AppEnv (Scene)
evalScene scene = 
  do let s = parseScene $ scene
         -- code = T.walkTreesWhere (T.isOscPattern . T.applied_as) (parsed s)
         code = T.walkTreesWhere (T.isBits . T.applied_as)
           (parsed s)
     -- liftIO $ T.printDists $ parsed s
     -- liftIO $ mapM_ (\d -> putStrLn $ T.token d ++ ": " ++ (show $ T.applied_as d)) (filter (not . T.hasParent) $ parsed s)
     if (null code)
       then (return s)
       else (
         do -- TODO Only one action I guess..
           let code' = head code
           i <- input `liftM` ask
           -- liftIO $ putStrLn $ "sending '" ++ code' ++ "'"
           liftIO $ putMVar i (WeaveJob code')
           -- s' <- interpretPats s
           -- return s'
           return s
         )

nextIdent :: [TWord] -> Int
nextIdent ws = head $ filter (\i -> null $ filter ((== i) . ident) ws) [0 ..]

wordToDatum :: TWord -> T.Datum
wordToDatum w = T.Datum {T.ident = ident w,
                         T.token = token w,
                         T.location = location w,
                         T.parentId = Nothing,
                         T.childIds = [],
                         T.sig  = s,
                         T.applied_as = s,
                         T.applied_location = bottomRight w
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

withSource :: Scene -> ([TWord] -> [TWord]) -> Scene
withSource s f = s {source = f $ source s}

withActive :: Scene -> ([TWord] -> [TWord]) -> Scene
withActive s f = withSource s (f . filterActive)

filterActive :: [TWord] -> [TWord]
filterActive = (filter ((Prelude.flip (elem) [Active,ActiveSelected]) . status))

clearMouseOffset :: [TWord] -> [TWord]
clearMouseOffset = (map clearTentative) . (map f) . (filter filtF)
  where f i = i {mousePos = Nothing}
        filtF i@(TWord {status = Tentative, location = (x,_), size = (w,_)}) 
          = (x + w) <= xDivider
        filtF i@(TWord {status = Active, location = (x,_), size = (w,_)}) 
          = (x + w) <= xDivider
        filtF _ = True
        clearTentative i@(TWord {status = Tentative}) = i {status = Active}
        clearTentative i = i

nudgeWords :: Scene -> IO Scene
--nudgeWords s = foldr (nudgeWord) s (source s)
nudgeWords s = do g <- newStdGen
                  let as = map ((1/4)-) $ randomRs (0,(1/2) :: Float) g
                  return $ s {source = map (nudgeWord) (zip (source s) as)}

nudgeWord :: (TWord, Float) -> TWord
nudgeWord (w,r) | energy w <= 0 = w
                | otherwise = bounceWord dir $ w {location = (x,y),
                                                  energy = energy w - (energy w * 0.25)
                                                 }
  where dir = (r + angle w) * (pi * 2)
        ox = (cos dir) * (energy w * 0.25)
        oy = (sin dir) * (energy w * 0.25)
        x = (fst $ location w) + ox
        y = (snd $ location w) + oy

bounceWord dir w@(TWord {location = (x,y), angle = a})
  | x < 0 = bounceWord dir $ w {location = (0-x,y), angle = a + (0 - dir)}
  | x >= 1 = bounceWord dir $ w {location = (1-(x-1),y), angle = a + (pi - dir)}
  | y < 0 = bounceWord dir $ w {location = (x,0-y), angle = a + (pi/2 - dir)}
  | y >= 1 = bounceWord dir $ w {location = (x,1-(y-1)), angle = a + (pi*1.5 - dir)}
  | otherwise = w

{-
if (y < 0) {
      y = 0 - y;
      angle += (HALF_PI) - dir;
    }
    if (y >= height) {
      y = height - (y -height);
      angle += (PI * 1.5) - dir;
    }
-}
moveWord :: Scene -> (Float, Float) -> AppEnv Scene
moveWord s (x,y) | w == Nothing = return s
                 | otherwise = 
                   do mods <- liftIO getModState  
                      let ctrl = shiftDown mods
                      s' <- moveWord' ctrl s (x,y) (fromJust w)
                      return $ parseScene $ s'
  where w = moving $ source s

moveWord' :: Bool -> Scene -> (Float, Float) -> TWord -> AppEnv Scene
moveWord' ctrl s loc wd@(TWord {status = MenuItem}) = moveWord' ctrl (s {source = ws'}) loc newWord
  where ws = source s
        newWord = wd {status = Tentative, ident = nextIdent ws}
        ws' = newWord:(clearMouseOffset ws)
        
moveWord' True s (x,y) w = do -- liftIO $ putStrLn $ show (map ident movingWs)
                              move s movingWs
           where ds = parsed s
                 ws = source s
                 d = T.datumByIdent (ident w) ds
                 movingDs = reverse $ d:(T.offspring ds d)
                 movingWs = map ((wordByIdent ws) . T.ident) movingDs
                 move s [] = return s
                 move s (w:ws) = do s' <- moveWord' False s (x,y) w
                                    move s' ws
    
moveWord' False s (x,y) wd =
  do -- liftIO $ putStrLn $ show $ "foo: " ++ (show $ mousePos $ fromJust $ moving ws)
     return $ s {source = ws'}
       where ws = source s
             ws' = fromMaybe ws $ 
                   -- distance from word which has mousepos..
                   do --(xOffset, yOffset) <- mousePos wd
                      parent <- moving ws
                      (pxOffset, pyOffset) <- mousePos parent
                      let (w,h) = size parent
                          (parentX, parentY) = location parent
 --                         parentX' | status parent == Tentative = (x-pxOffset)
--                                   | otherwise = max 0 $ min (xDivider - w) (x - pxOffset)
                          parentX' = max 0 $ min (1 - w) $ x-pxOffset
                          parentY' = max 0 $ min (1 - h) $ y - pyOffset
                          moveX = parentX' - parentX
                          moveY = parentY' - parentY
                          (childX, childY) = location wd
                          x' = childX + moveX
                          y' = childY + moveY
                      return $ updateWord ws $ wd {location = (x',y')}

inWord :: (Float, Float) -> TWord -> Bool
inWord (px,py) TWord {size = (w,h), location = (x,y)} =
  and [px >= x, py >= y, px < x+w, py < y+h]

instructionAt :: [TWord] -> (Float, Float) -> Maybe TWord
instructionAt ws location = listToMaybe $ filter (inWord location) ws

moving :: [TWord] -> Maybe TWord
moving = listToMaybe . (filter (isJust . mousePos))

updateAddWord :: [TWord] -> TWord -> [TWord]
updateAddWord [] w = [w]
updateAddWord (x:xs) i | ident x == ident i = (i:xs)
                       | otherwise = x:(updateAddWord xs i)

updateWord :: [TWord] -> TWord -> [TWord]
updateWord [] _ = []
updateWord (x:xs) i | ident x == ident i = (i:xs)
                    | otherwise = x:(updateWord xs i)

removeWord :: [TWord] -> TWord -> [TWord]
removeWord ws w = filter (w /=) ws

wordByIdent :: [TWord] -> Int -> TWord
wordByIdent ds i = head $ filter (\d -> ident d == i) ds

wordByIdent' :: [TWord] -> Int -> [TWord]
wordByIdent' ds i = filter (\d -> ident d == i) ds

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y = (x' > rx) && (x' < rx + rw) && (y' > ry) && (y' < ry + rh)
 where (x', y') = (fromIntegral x, fromIntegral y)

setMouseOffset :: (Float, Float) -> TWord -> TWord
setMouseOffset (x,y) w = w {mousePos = Just (x',y')}
  where x' = x - (fst $ location w)
        y' = y - (snd $ location w)

ctrlDown mods = or $ map (\x -> elem x [KeyModLeftCtrl, 
                                        KeyModRightCtrl
                                       ]
                         ) mods

shiftDown mods = or $ map (\x -> elem x [KeyModLeftShift, 
                                         KeyModRightShift,
                                         KeyModShift
                                        ]
                          ) mods

handleEvent :: Scene -> Graphics.UI.SDL.Event -> AppEnv (Scene)
handleEvent s (MouseMotion x y _ _) = 
  do s' <- moveWord s xy
     return $ parseScene (s' {mouseXY = xy})
  where xy = (fromScreen (fromIntegral x, fromIntegral y))

--return $ parseScene $ source $ withSource scene (\ws -> moveWord ws (fromScreen (fromIntegral x, fromIntegral y)))

handleEvent scene (MouseButtonDown x y ButtonLeft) = 
  do scene' <- finishTyping scene
     mods <- liftIO getModState
     -- liftIO $ putStrLn $ show mods
     let word = clicked scene'
         startEdit = False -- isJust word && ctrlDown mods
         word' = do w <- word
                    let w' | ctrlDown mods = w {status = Typing}
                           | otherwise = w
                    return $ setMouseOffset xy w'
         scene'' | startEdit = updateCursor (fromJust word') scene'
                 | otherwise = scene'
     return $ updateScene scene'' word'
  where clicked scene = instructionAt (source scene) xy
        updateScene scene Nothing = scene {cursor = xy}
        updateScene scene (Just w) = scene {source = updateWord (source scene) w}
        xy = fromScreen (fromIntegral x, fromIntegral y)

handleEvent scene (MouseButtonUp x y ButtonLeft) = evalScene $ withSource scene clearMouseOffset

handleEvent scene (KeyDown k) =
     handleKey scene (symKey k) (symUnicode k) (symModifiers k)

handleEvent scene _ = return scene

typing :: Scene -> TWord
typing scene = 
  fromMaybe (makeTyping (source scene) (cursor scene)) 
            (getTyping (source scene))

finishTyping :: Scene -> AppEnv (Scene)
finishTyping scene | w == Nothing = return scene
                   | otherwise = evalScene $ parseScene $ withSource scene (\ws -> updateWord ws ((fromJust w) {status = Active}))
  where w = getTyping (source scene)

updateSize :: Int -> Scene -> AppEnv Scene
updateSize i scene = 
  do ft <- font `liftM` ask
     w' <- liftIO (setSize w ft)
     return $ updateCursor w' $ withSource scene (\ws -> updateWord ws w')
  where w = wordByIdent (source scene) i

updateCursor :: TWord -> Scene -> Scene
updateCursor w scene = scene {cursor = topRight w}

handleKey :: Scene -> SDLKey -> Char -> [Modifier] -> AppEnv Scene

handleKey scene SDLK_SPACE _ _ = 
  finishTyping $ scene {cursor = addBlank $ topRight (typing scene)}
  where addBlank (x,y) = (x+blankWidth,y)

handleKey scene SDLK_RETURN _ _ = 
  finishTyping $ scene {cursor = bottomLeft (typing scene)}

handleKey scene SDLK_BACKSPACE _ _ = 
  deleteChar $ getTyping (source scene)
  where deleteChar (Just w) | length (token w) > 1 = updateSize (ident w) $ withSource scene (\ws -> updateWord ws (w {token = Prelude.init (token w)}))
                            | otherwise = return $ withSource scene (\ws -> removeWord ws w)
        deleteChar Nothing = return $ scene {cursor = removeBlank $ cursor scene}
        removeBlank (x,y) = (x-blankWidth,y)        

handleKey scene SDLK_DELETE c mods = handleKey scene SDLK_BACKSPACE c mods

handleKey scene SDLK_RIGHT _ _ =
  do loomM <- loomMV `liftM` ask
     bumpLoom loomM 1
     return scene

handleKey scene SDLK_LEFT _ _ =
  do loomM <- loomMV `liftM` ask
     bumpLoom loomM (-1)
     return scene

handleKey scene SDLK_UP _ _ =
  do loomM <- loomMV `liftM` ask
     bumpLoom loomM 0
     return scene

handleKey scene k c mods
  | isKey c = do let w = (typing scene) 
                     w' = w {token = (token w) ++ [c]}
                 updateSize (ident w') $ withSource scene (\ws -> updateAddWord ws w')
  | otherwise = do liftIO $ putStrLn $ [c] ++ " " ++ show k
                   return scene

bumpLoom :: MVar L.Loom -> Int -> AppEnv ()
bumpLoom loomM n = do tempoM <- tempoMV `liftM` ask
                      tempo <- liftIO $ readMVar tempoM
                      now <- O.time
                      let cyc = timeToCycles tempo now
                          q = 1
                          s = floor cyc
                          prev = s - (s `mod` q)
                          next = prev + q
                          cycDelay = (toRational $ (s - (s `mod` q)) + q) - cyc
                          secDelay = cycDelay / (toRational $ cps tempo)
                          toDouble :: Rational -> Double
                          toDouble = fromRational
                      liftIO $ putStrLn $ "delay: " ++ show (toDouble secDelay) ++ " cyc: " ++ show (toDouble cyc) ++ " next: " ++ show next ++ " prev: " ++ show prev
                      liftIO $ threadDelay $ floor (secDelay * 1000000)
                      liftIO $ do loom <- takeMVar loomM
                                  let row = max 0 $ (L.lRow loom) + n
                                  let loom' = loom {L.lRow = row}
                                  L.sendRow (toRational $ cps tempo) loom'
                                  putMVar loomM $ loom'
                      return ()

isKey c = elem c s
  where s = concat [['a' .. 'z'],
                    ['A' .. 'Z'],
                    ['0' .. '9'],
                    "!\"£$%^&*()[]~@:#;?></.,|{}=+-_\\"
                   ]


resetPats :: [TWord] -> [TWord]
resetPats = map (\w -> w {pat = Nothing})

{-
interpretPats :: Scene -> AppEnv Scene
interpretPats s = do ps <- pats
                     metaPs <- metaPats
                     let ws = foldr (Prelude.flip updateWord) (resetPats $ source s) (ps ++ metaPs)
                     return $ s {source = ws}
  where isBitted x = T.hasParent x && T.isBits (T.appliedConcreteType x)
        bitted = (filter isBitted $ parsed s) :: [T.Datum]
        simpleJob d = runJob (d, (fromJust' $ T.patternType $ T.appliedConcreteType d), T.walkTree (parsed s) d)
        runJob (d, t, code) = 
          do let job = ColourJob t code
             i <- input `liftM` ask
             o <- colourOutput `liftM` ask
             liftIO $ putMVar i job
             p <- liftIO $ takeMVar o
             let w = wordByIdent (source s) (T.ident d)
             return $ w {pat = p}
        pats = mapM simpleJob patterned
        metas :: [(T.Datum, T.Type, String)]
        metas = catMaybes $ map (T.guessTransform (parsed s)) (parsed s)
        metaPats = mapM runJob metas
-}

data AppConfig = AppConfig {
  screen       :: Surface,
  font         :: Font,
  input        :: MVar Job,
  -- oscOutput    :: MVar (ParamPattern),
  colourOutput :: MVar (Maybe (Pattern (Colour Double))),
  loomMV       :: MVar L.Loom,
  tempoMV      :: MVar Tempo,
  fr           :: FR.FPSManager
  -- mxyz         :: MVar (Float, Float, Float),
  -- mEnergy      :: MVar Float
}

type AppState = StateT Scene IO
type AppEnv = ReaderT AppConfig AppState

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

initEnv :: IO AppConfig
initEnv = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    font <- openFont "futura.ttf" 22
    setCaption "Texture" []
    tidal <- startTidal (superdirtTarget {oLatency = 0.15, oAddress = "127.0.0.1", oPort = 57120}) defaultConfig
    let oscO = streamReplace tidal 1
    -- _ <- carabiner tidal 4 0
    colourO <- newEmptyMVar
    loom <- L.loom
    loomO <- newMVar loom
    i <- Texture.Interp.start oscO colourO loomO
    let tempoMV  = sTempoMV tidal
    fps <- FR.new
    FR.set fps 20
    FR.init fps
    return $ AppConfig screen font i colourO loomO tempoMV fps

blankWidth = 0.015

{-
oscThread = do x <- udpServer myIP 1234
               mv <- newMVar (0,0,0) 
               mve <- newMVar (0) 
               forkIO $ oscLoop x mv mve
               return (mv, mve)
  where oscLoop x mv mve = do m <- recvMessage x
                              act m mv mve
                              oscLoop x mv mve
        act (Just (O.Message "/kill" [])) mv mve = 
          do putMVar mv (-1,-1,-1)
             return ()
        act (Just (O.Message "/energy" [O.Float x])) mv mve = 
          do putMVar mve (x)
             -- putStrLn "energy"
             return ()
        act (Just (O.Message "/isadora/1" [O.Float x, O.Float y, O.Float z])) mv mve =
          do -- putStrLn $ "got :" ++ show x ++ ", " ++ show y ++ ", " ++ show z
             putMVar mv (x,y,z)
             return ()
        act _ _ _ = do putStrLn "hm"
                       return ()
-}

drawCursor :: Scene -> Font -> Surface -> Sound.Tidal.Pattern.Time -> IO ()
drawCursor scene ft screen cyc = 
  do  
     let colour  = rgbColor (foo r) (foo g) (foo b)
     fillRect screen (Just $ Rect x y w h) colour
     return ()
  where (x,y) = toScreen $ cursor scene
        (w,h) = toScreen (blankWidth * 1.2, blankWidth * 1.8)
        RGB r g b = hsv (hu*360) 0.7 0.99999
        foo x = floor $ x * 256
        hu = ((fromRational cyc) `mod'` 1)


rowWindow :: L.Loom -> (Int, [[Bool]])
rowWindow loom = (L.lRow loom - start, rows)
  where showRows = 12
        start = max 0 (L.lRow loom - (showRows-6))
        rows = take showRows $ drop start $ L.rows loom

drawLoom :: Scene -> Font -> Surface -> L.Loom -> IO ()
drawLoom scene font screen loom = 
  do let (current, rows) = rowWindow loom
     sequence_ $ map (drawLoomRow current) $ enumerate $ rows
     let text = "row " ++ show (L.lRow loom)
     message <- renderTextShaded font text (Color 255 255 255) (Color 0 0 0)
     applySurface 
       (toSx $ xDivider + border)
       (toSy $ 0.9)
       message screen Nothing
     return ()
  where drawLoomRow current (y', bits) =
          do when (y' == current) $
               do fillRect screen (Just $ Rect ((x-cellWidth)+ (toSx border)) (y+(y'*cellWidth)) (cellWidth*18) cellWidth) selected
                  return ()
             sequence_ $ map (drawLoomCell y') $ enumerate bits
                                    
        drawLoomCell y' (x', bit) = do fillRect screen (Just $ Rect (x+(x'*cellWidth)+ (toSx border)) (y+(y'*cellWidth)) cellWidth cellWidth) $ if bit then foreground else background
                                       -- fillRect screen (Just $ Rect (x+(x'*cellWidth)+ (toSx border)) (y+(y'*cellWidth) + (toSy 0.23)) cellWidth cellWidth) $ if bit then (if (even y') then threada else threadb) else (if (even x') then threadb else threada)
        foreground  = (Pixel 0x00ffffff)
        background  = (Pixel 0x00000000)
        threada  = (Pixel 0x00ffff00)
        threadb  = (Pixel 0x000000ff)
        selected  = (Pixel 0x000000ff)
        (x,y) = toScreen (xDivider, 0.55)
        foo a = floor $ a * 256
        border = 0.02
        cellWidth = toSx $ (1-(xDivider+(border*2)))
                           / (fromIntegral W.width)
        toSx a = floor $ a * (fromIntegral screenWidth)
        toSy a = floor $ a * (fromIntegral screenHeight)

drawScene :: Scene -> Font -> Surface -> Sound.Tidal.Pattern.Time -> L.Loom -> IO ()
drawScene scene font screen cyc loom = 
  do mapM_ (drawTree scene font screen cyc) top
     mapM_ (\i -> 
             do let (x, y) = toScreen $ location i
                    (w, h) = toScreen $ size i
                fillRect screen (Just $ Rect x y w h) (Pixel 0x00333333)
                message <- renderTextShaded font (token i) textColor (Color 51 51 51)
                applySurface 
                  (floor $ (fromIntegral screenWidth) * (fst $ location i)) 
                  (floor $ (fromIntegral screenHeight) * (snd $ location i)) 
                  message screen Nothing
           ) (source scene)
     drawLoom scene font screen loom
     drawCursor scene font screen cyc
  where top = filter (T.hasChild) $ parsed scene
        textColor = Color 255 255 255

drawTree :: Scene -> Font -> Surface -> Sound.Tidal.Pattern.Time -> T.Datum -> IO ()
drawTree scene font screen cyc d 
 | T.token d == "[" = mapM_ drawPlainLink links
 | T.token d == "]" = mapM_ drawPlainLink links
 | otherwise = 
--  do mapM_ drawLink links
  do mapM_ drawPlainLink links
  where links = tails $ d:(T.children (parsed scene) d)
        drawLink [] = return ()
        drawLink (_:[]) = return ()
        drawLink ds@(a:b:_) = do mapM_ (drawLinkLine a b) (enumerate $ reverse $ tail ds)
        drawPlainLink [] = return ()
        drawPlainLink (_:[]) = return ()
        drawPlainLink ds@(a:b:_) = do drawLinkLine a b (0, b)
        drawLinkLine a b (n, x) = 
          drawPat n x1 y1 x2 y2 p screen cyc
          where (x1, y1) | a == d = bottomRight w
                         | otherwise = T.location a
                (x2, y2) = T.location b
                p = pat $ wordByDatum (source scene) x
        w = wordByDatum (source scene) d

wordByDatum :: [TWord] -> T.Datum -> TWord
wordByDatum ws d = wordByIdent ws (T.ident d) 
 
drawPat :: Int -> Float -> Float -> Float -> Float -> Maybe (Pattern (Colour Double)) -> Surface -> Rational -> IO ()
drawPat n x1 y1 x2 y2 (Nothing) screen _ = 
--  do (thickLine True n linesz x2 y2 x1 y1) screen lineColor
  do (thickLineArrow n 0.009 x1 y1 x2 y2) screen lineColor
     return ()
  where lineColor = rgbColor 240 240 255

drawPat n x1 y1 x2 y2 (Just p) screen cyc = do mapM_ drawEvents es
                                               --drawArc n x1 y1 x2 y2 (Just p) screen cyc
  where es = map (\ev@(Event _ _ (Arc s e) evs) -> ((max s cyc, min e (cyc + 1)),evs)) $ queryArc (segment2 p) (Arc cyc (cyc + 1))
        constrain x = min (cyc + 1) $ max x cyc
        xd = x2 - x1
        yd = y2 - y1
        drawEvents ((s,e), cs) = 
          mapM_ (\(n', (h, c)) -> drawEvent h (s,e) c n' (length cs)) (enumerate cs)
        drawEvent h (s,e) c n' scale = 
          (thickLine h (n*scale+n') (linesz/ (fromIntegral scale))
           (x1 + (xd * fromRational (e-cyc)))
           (y1 + (yd * fromRational (e-cyc)))
           (x1 + (xd * fromRational (s-cyc))) 
           (y1 + (yd * fromRational (s-cyc)))
          ) 
          screen (colourToPixel c)

drawArc n x1 y1 x2 y2 Nothing screen cyc = return ()
drawArc n x1 y1 x2 y2 (Just p) screen cyc = 
  do let s = cyc
         e = s + (1%20)
         now = queryArc p (Arc s e)
         colours = enumerate $ map (colourToPixel . value) now
         parts = length colours
     mapM_ (drawPie (fromIntegral parts)) (mapFsts fromIntegral colours)
     return ()
       where (x,y) = toScreen16 (x1,y1)
             drawPie parts (i, c) = SDLP.filledPie screen x y 10 (floor $ ((fromIntegral i)/(fromIntegral parts))*360) (floor $ (((fromIntegral i)+1)/(fromIntegral parts))*360) c
             mapFst :: (a -> b) -> (a, c) -> (b, c)
             mapFst f (x,y) = (f x,y)
             mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
             mapFsts = map . mapFst

segment2 :: Pattern a -> Pattern [(Bool, a)]
segment2 p = Pattern $ \(Sound.Tidal.Pattern.State (Arc s e) _) -> filter (\(Event _ _ (Arc s' e') _) -> s' < e && e' > s) $ groupByTime (segment2' (queryArc (fmap (\x -> (True, x)) p) (Arc s e)))

groupByTime :: [Sound.Tidal.Pattern.Event a] -> [Sound.Tidal.Pattern.Event [a]]
groupByTime es = map mrg $ groupBy ((==) `on` part) $ sortBy (compare `on` part) es
  where mrg es@(e:_) = e {value = map value es}
  
segment2' :: [Sound.Tidal.Pattern.Event (Bool, a)] -> [Sound.Tidal.Pattern.Event (Bool, a)]
segment2' es = foldr split2 es pts
  where pts = nub $ points es
        points [] = []
        points ((Event _ _ (Arc s e) _):es) = s:e:(points es)

split2 :: Sound.Tidal.Pattern.Time -> [Sound.Tidal.Pattern.Event (Bool, a)] -> [Sound.Tidal.Pattern.Event (Bool, a)]
split2 _ [] = []
split2 t ((ev@(Event c a (Arc s e) (h,v))):es) | t > s && t < e = (Event c a (Arc s t) (h,v)):(Event c a (Arc t e) (False,v)):(split t es)
                                               | otherwise = ev:split2 t es

split :: Sound.Tidal.Pattern.Time -> [Sound.Tidal.Pattern.Event a] -> [Sound.Tidal.Pattern.Event a]
split _ [] = []
split t ((ev@(Event c a (Arc s e) v)):es) | t > s && t < e = (Event c a (Arc s t) v):(Event c a (Arc t e) v):(split t es)
                                          | otherwise = ev:split t es

thickLine :: Bool -> Int -> Float -> Float -> Float -> Float -> Float -> (Surface -> Pixel -> IO ())
thickLine h n thickness x1 y1 x2 y2 = 
  \s p -> do SDLP.filledPolygon s coords p
             SDLP.aaPolygon s coords p
             line s
             return ()
  where x = x2 - x1
        y = y2 - y1
        l = sqrt $ x*x+y*y
        line s | h = do line' s
                        return ()
               | otherwise = return ()
        line' s = SDLP.aaLine s (fst $ coords !! 1) (snd $ coords !! 1) (fst $ coords !! 2) (snd $ coords !! 2) (Pixel 0x000000ff)
        incX = (fromIntegral n) * (thickness * (y2-y1) / l)
        incY = (fromIntegral n) * (thickness * (x1-x2) / l)
        ox = (thickness * (y2-y1) / l)
        oy = (thickness * (x1-x2) / l)
        coords = map toScreen16 [(x1 + ox + incX, y1 + oy + incY),
                                 ((x2 + ox + incX), (y2 + oy + incY)),
                                 (((x2 ) + incX), ((y2) + incY)),
                                 (x1 + incX , y1 + incY)
                                ]

thickLineArrow :: Int -> Float -> Float -> Float -> Float -> Float -> (Surface -> Pixel -> IO Bool)
thickLineArrow n thickness x1 y1 x2 y2 = 
  \s p -> do SDLP.filledPolygon s (map toScreen16 coords) p
             SDLP.aaPolygon s (map toScreen16 coords) p
             SDLP.filledPolygon s (map toScreen16 arrowCoords) p
             SDLP.aaPolygon s (map toScreen16 arrowCoords) p
  where x = x2 - x1
        y = y2 - y1
        headx = (x/l) / 60
        heady = (y/l) / 60
        l = sqrt $ x*x+y*y
        incX = (fromIntegral n) * (thickness * (y2-y1) / l) * 2
        incY = (fromIntegral n) * (thickness * (x1-x2) / l) * 2
        ox = (thickness * (y2-y1) / l)/2
        oy = (thickness * (x1-x2) / l)/2
        coords = [(x1 + ox + incX, y1 + oy + incY),
                  ((x2 + ox + incX) - headx, (y2 + oy + incY) - heady),
                  (((x2 - ox) + incX) - headx, ((y2 - oy) + incY) - heady),
                  (x1 + incX - ox, y1 + incY - oy)
                 ]
        arrowCoords = [(((x2 + ox) + incX) - headx, 
                        ((y2 + oy) + incY) - heady),
                       (((x2 - ox) + incX) - headx,
                        ((y2 - oy) + incY) - heady),
                       (x2+incX, y2+incY)
                      ]

{-
energise :: AppEnv ()
energise = do m <- mEnergy `liftM` ask
              scene <- get
              e <- liftIO $ tryTakeMVar m
              case e of Just v -> do let scene' = scene {source = map (addEnergy v) (source scene)}
                                     evalScene scene' >>= put
                                     return ()
                        Nothing -> return ()
  where addEnergy e w = w {energy = (energy w) + e}
-}

{-
moveKate :: AppEnv ()
moveKate = do kate <- mxyz `liftM` ask
              scene <- get
              xyz <- liftIO $ tryTakeMVar kate
              case xyz of Just (-1,-1,-1) -> do let scene' = Scene [] [] (0,0) (0.5,0.5)
                                                scene'' <- evalScene scene'
                                                put scene''
                                                return ()
                          Just (xyz') -> do -- liftIO $ putStrLn "got xyz"
                                            let scene' = parseScene $ scene {source = setFirstXYZ (source scene) xyz'}
                                            scene'' <- evalScene scene'
                                            put scene''
                                            return ()
                          Nothing -> return ()
  where setFirstXYZ [] _ = []
        setFirstXYZ (ws) (x,y,z) | status w == Active = updateWord ws $ w {location = (x,y)}
        
                                 | otherwise = ws
          where w' = wordByIdent' ws 0
                w | w' == [] = head ws
                  | otherwise = head w'
-}

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ act
    --moveKate
    -- energise
    screen <- screen `liftM` ask
    font <- font `liftM` ask
    tempoM <- tempoMV `liftM` ask
    fps <- fr `liftM` ask
    loomM <- loomMV `liftM` ask
    loom <- liftIO $ readMVar loomM
    tempo <- liftIO $ readMVar tempoM
    now <- O.time
    let cyc = timeToCycles tempo now
    preNudged <- get
    scene <- liftIO $ (nudgeWords preNudged) >>= return . parseScene
    put $ scene
    liftIO $ do
        bgColor  <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00  
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect bgColor
        SDLP.aaLine screen (floor $ xDivider * (fromIntegral screenWidth)) 0 (floor $ xDivider * (fromIntegral screenWidth)) (fromIntegral screenHeight) (Pixel 0x00ffffff)
        drawScene scene font screen cyc loom
        Graphics.UI.SDL.flip screen
        FR.delay fps
    unless quit loop
      where act e = do scene <- get 
                       scene' <- handleEvent scene e
                       put scene'

whileEvents :: MonadIO m => (Graphics.UI.SDL.Event -> m ()) -> m Bool
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

newWord :: Int -> String -> (Float, Float) -> Font -> WordStatus -> IO (TWord)
newWord ident text location font status = setSize wd font
  where wd = TWord ident text location undefined Nothing status Nothing 0 0

setSize :: TWord -> Font -> IO TWord
setSize wd@(TWord {token = ""}) _ = return wd {size = (0,0)}
setSize wd font = do sz <- textSize (token wd) font
                     return $ wd {size = sz}

wordMenu :: Font -> [String] -> IO ([TWord])
wordMenu font ws = mapM addWord (enumerate ws)
  where addWord (n, w) = 
          newWord n w (xDivider + 0.005 + ((fromIntegral $ n `mod` 2) * (1 - xDivider) / 2), (fromIntegral (n `div` 2)) * 0.037) font MenuItem

things = (map fst T.functions)

run = withInit [InitEverything] $ 
      do result <- TTFG.init
         if not result
           then putStrLn "Failed to init ttf"
           else do enableUnicode True
                   env <- initEnv
                   ws <- wordMenu (font env) things
                   let scene = parseScene $ Scene ws [] (0,0) (0.5,0.5)
                   --putStrLn $ show scene
                   runLoop env scene


colourToPixel :: Colour Double -> Pixel
colourToPixel c =  rgbColor (floor $ 256*r) (floor $ 256*g) (floor $ 256*b)
  where (RGB r g b) = toSRGB c

fi a = fromIntegral a

rgbColor :: GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8 -> Pixel
rgbColor r g b = Pixel (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255))

pixel :: Surface -> (GHC.Word.Word8,GHC.Word.Word8,GHC.Word.Word8) -> IO Pixel
pixel surface (r,g,b) = mapRGB (surfaceGetPixelFormat surface) r g b
