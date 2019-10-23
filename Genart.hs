{-# LANGUAGE TypeApplications #-}

module Main where

import           Codec.Picture            (PixelRGBA8( .. ), writePng, Image)
import           Control.Arrow
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.List                (nub)
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import           Graphics.Rasterific
import           Graphics.Rasterific.Linear
import           Graphics.Rasterific.Texture
import           Graphics.Rasterific.Transformations
import qualified Numeric.Noise.Perlin     as P
import           System.IO
import Data.Char
import Data.String
import Text.Read

-- VALUES
-- to be passed into createArt function

-- Values for Shape
triVal :: Float
triVal = 0.0

sqVal :: Float
sqVal = 1.5

lineVal :: Float
lineVal = 100

barVal :: Float
barVal = 5

modernVal1 :: Float
modernVal1 = 15

modernVal2 :: Float
modernVal2 = 9

-- Values for Colour (c3, c4)
orange :: Int
orange = 40

cRed :: Double
cRed = 10

cOrange :: Double
cOrange = 30

cYellow :: Double 
cYellow = 50

cGreen :: Double
cGreen = 100

cBabyBlue :: Double
cBabyBlue = 200

cDarkBlue :: Double
cDarkBlue = 255

cPink :: Double
cPink = 300

--
-- Warns the user of invalid Input
-- 
sanitation :: [Char] -> [(Char -> Bool)] -> IO () 
sanitation input [] = putStrLn ("\n")
sanitation input (h:t)  = do
 if (input == []) then do {
  ; putStrLn $ "\x1b[31m"
  ; putStrLn("\nInvalid input. Try again!\n")
  ; putStrLn $ "\x1b[30m"
  ; newInput <- getLine
  ; sanitationHelper newInput (h:t) 
  }  
  else if (not (h $ head input)) then do {
   ; putStrLn $ "\x1b[31m"
   ; putStrLn("\nInvalid input. Try again!")
   ; putStrLn $ "\x1b[30m"

   ; newInput <- getLine
   ; sanitationHelper newInput (h:t)
   } 
   else do {
    ; sanitationHelper input t
  }

sanitationHelper :: String -> [Char -> Bool] -> IO ()
sanitationHelper input [] = putStrLn ("\n")
sanitationHelper input (h:t) = do
 if (((length input) > 4)) then do {
  ; putStrLn $ "\x1b[31m"
  ; putStrLn("\nInvalid entry. Try again!\n")
  ; putStrLn $ "\x1b[30m"
  ; newInput <- getLine
  ; sanitationHelper newInput (h:t)
 }
 else sanitation input (h:t)

-- 
-- Checks to make sure the input is no more than 4 characters, lessthan /equal to 1000
--
sanitationHelper800 :: String -> [Char -> Bool] -> IO ()
sanitationHelper800 input [] = putStrLn ("\n")
sanitationHelper800 input (h:t) = do
 if (((length input) > 4) || ((read input > 800)) || (read input == 0)) then do {
  ; putStrLn $ "\x1b[31m"
  ; putStrLn("\nInvalid entry. Try again!\n")
  ; putStrLn $ "\x1b[30m"
  ; newInput <- getLine
  ; sanitationHelper800 newInput (h:t)
 }
 else sanitation input (h:t)

sanitationHelper255 :: String -> [Char -> Bool] -> IO ()
sanitationHelper255 input [] = putStrLn ("\n")
sanitationHelper255 input (h:t) = do
 if (((length input) > 3) || ((read input > 255))) then do {
  ; putStrLn $ "\x1b[31m"
  ; putStrLn("\nInvalid entry. Try again!\n")
  ; putStrLn $ "\x1b[30m"
  ; newInput <- getLine
  ; sanitationHelper255 newInput (h:t)
 }
 else sanitation input (h:t)



main :: IO ()
main = do

  ---------------------SANITATION BEGIN----------------------
  -----------------------------------------------------------
  putStrLn $ "\x1b[32m"
  putStrLn("T I M E  T O  C R E A T E  A R T")
  putStrLn $ "\x1b[30m"

  putStrLn $ "\x1b[34m"
  putStrLn("\nA number between 0 and 255 inclusive\n")
  putStrLn $ "\x1b[30m"
  backGroundColor <- getLine
  sanitationHelper255 backGroundColor [isDigit, (`elem` (map chr[0..255]))]

  putStrLn $ "\x1b[34m"
  putStrLn("\nPick a number between 1 and 800 inclusive\n")
  putStrLn $ "\x1b[30m"
  numQuads <- getLine
  sanitationHelper800 numQuads [isDigit, (`elem` (map chr[1..800]))]

  putStrLn $ "\x1b[34m"
  putStrLn("\nPick a color by choosing a number:\n1. Red\n2. Blue\n")
  putStrLn $ "\x1b[30m"
  c3 <- getLine
  
  putStrLn $ "\x1b[34m"
  putStrLn("\nPick another color by choosing a number:\n1. Yellow\n2. Green")
  putStrLn $ "\x1b[30m"
  c4 <- getLine

  ---
  ---
  ---
  ---
  ---
  ---
  --- ENTERING FINAL INPUT TO CREATE GENERATIVE ART
  ---
  ---
  ---
  ---
  ---
  ---
  putStrLn $ "\x1b[34m"
  putStrLn("\nPick a style:\n1. Squares\n2. Triangles\n3. Lines\n4. Smudged\n5. ~ M O D E R N ~")
  putStrLn $ "\x1b[30m"
  style <- getLine
  
  if(head style == '1' && head c3 == '1' && head c4 == '1')  --SQUARE, RED and YELLOW
  then do
    createArt cRed cYellow sqVal sqVal (read numQuads) (read backGroundColor)
  else if(head style == '1' && head c3 == '1' && head c4 == '2')  --SQUARE RED and GREEN
  then do
    createArt cRed cGreen sqVal sqVal (read numQuads) (read backGroundColor)
  else if(head style == '1' && head c3 == '2' && head c4 == '1')  --SQUARE BLUE and YELLOW
  then do
    createArt cBabyBlue cYellow sqVal sqVal (read numQuads) (read backGroundColor)
  else if(head style == '1' && head c3 == '2' && head c4 == '2')  --SQUARE BLUE and GREEN
  then do
    createArt cBabyBlue cGreen sqVal sqVal (read numQuads) (read backGroundColor)
  else if(head style == '2' && head c3 == '1' && head c4 == '1')  --TRIANGLE RED and YELLOW
  then do
    createArt cRed cYellow triVal sqVal(read numQuads) (read backGroundColor)
  else if(head style == '2' && head c3 == '1' && head c4 == '2')  --TRIANGLE RED and GREEN
  then do
    createArt cRed cGreen triVal sqVal(read numQuads) (read backGroundColor)
  else if(head style == '2' && head c3 == '2' && head c4 == '1')  --TRIANGLE BLUE and YELLOW
  then do
    createArt cBabyBlue cYellow triVal sqVal (read numQuads) (read backGroundColor)
  else if(head style == '2' && head c3 == '2' && head c4 == '2')  --TRIANGLE BLUE and YELLOW
  then do
    createArt cBabyBlue cGreen triVal sqVal (read numQuads) (read backGroundColor) --TRIANGLE BLUE and GREEN


  else if(head style == '3' && head c3 == '1' && head c4 == '1')  --LINES, RED and YELLOW
  then do
    createArt cRed cYellow lineVal triVal (read numQuads) (read backGroundColor)
  else if(head style == '3' && head c3 == '1' && head c4 == '2')  --LINES RED and GREEN
  then do
    createArt cRed cGreen lineVal triVal (read numQuads) (read backGroundColor)
  else if(head style == '3' && head c3 == '2' && head c4 == '1')  --LINES BLUE and YELLOW
  then do
    createArt cBabyBlue cYellow lineVal triVal (read numQuads) (read backGroundColor)
  else if(head style == '3' && head c3 == '2' && head c4 == '2')  --LINES BLUE and GREEN
  then do
    createArt cBabyBlue cGreen lineVal triVal (read numQuads) (read backGroundColor)
  else if(head style == '4' && head c3 == '1' && head c4 == '1')  --BARS RED and YELLOW
  then do
    createArt cRed cYellow sqVal barVal (read numQuads) (read backGroundColor)
  else if(head style == '4' && head c3 == '1' && head c4 == '2')  --BARS RED and GREEN
  then do
    createArt cRed cGreen sqVal barVal (read numQuads) (read backGroundColor)
  else if(head style == '4' && head c3 == '2' && head c4 == '1')  --BARS BLUE and YELLOW
  then do
    createArt cBabyBlue cYellow sqVal barVal (read numQuads) (read backGroundColor)
  else if(head style == '4' && head c3 == '2' && head c4 == '2')  --BARS BLUE and GREEN
  then do
    createArt cBabyBlue cYellow sqVal barVal (read numQuads) (read backGroundColor)
  else if(head style == '5' && head c3 == '1' && head c4 == '1')  --MODERN, RED and YELLOW
  then do
    createArt cRed cYellow modernVal1 modernVal2 (read numQuads) (read backGroundColor)
  else if(head style == '5' && head c3 == '1' && head c4 == '2')  --MODERN RED and GREEN
  then do
    createArt cRed cGreen modernVal1 modernVal2 (read numQuads) (read backGroundColor)
  else if(head style == '5' && head c3 == '2' && head c4 == '1')  --MODERN BLUE and YELLOW
  then do
    createArt cBabyBlue cYellow modernVal1 modernVal2 (read numQuads) (read backGroundColor)
  else do
    createArt cBabyBlue cGreen modernVal1 modernVal2 (read numQuads) (read backGroundColor) --MODERN BLUE and GREEN


  ---
  ---
  ---
  ---
  ---
  ---
  --- CREATE THE ART
  ---
  ---
  ---
  ---
  ---
  ---

createArt :: Double -> Double -> Float -> Float -> Int -> Double -> IO ()
createArt c3 c4 shape1 shape2 num color = do
  seed <- round . (*1000) <$> getPOSIXTime
  let stdGen  = mkStdGen seed
      width   = 40 --changed from 60 to 80
      height  = 40 --changed from 60 to 80
      scaling = 20 
      world   = World width height seed scaling (backGround(color) )
      img     = run world stdGen (render (sketch c3 c4 shape1 shape2 num))
      path    = "/Users/joshuamattson/Desktop/P1/images/sketch-" <> show seed <> "-"
             <> show (round scaling :: Int) <> ".png"

  putStrLn $ "\x1b[32m"          
  putStrLn "Generating art..."
  putStrLn $ "\x1b[30m"  
  writePng  path img
  writePng "/Users/joshuamattson/Desktop/P1/images/latest.png" img



data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Float
  , bgColor     :: PixelRGBA8
  }

type Generate a = RandT StdGen (Reader World) a

run :: World -> StdGen -> Generate a -> a
run w g gen = runReader (evalRandT gen g) w

hsva :: Double -> Double -> Double -> Double -> PixelRGBA8
hsva h s v a = PixelRGBA8 (w8 $ rC) (w8 gC) (w8 bC) (w8 a)
  where
    RGB rC gC bC = hsv h s v
    w8 x = round (255 * x)

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

render :: Generate (Drawing PixelRGBA8 ()) -> Generate (Image PixelRGBA8)
render d = do
  d' <- d
  World w h seed s bg <- ask
  let w' = round $ fromIntegral w * s
      h' = round $ fromIntegral h * s
  return $ renderDrawing w' h' bg (withTransformation (scale s s) d')

--------------------------------------------------------------------------------


backGround :: Double -> PixelRGBA8
backGround x        = hsva (x) 0.5 0.6 1.0

color1 :: Double -> PixelRGBA8
color1 x = hsva x 0.5 0.7 1.0

color2 :: Double -> PixelRGBA8
color2 x = hsva x 0.5 0.1 1.0

color3, color4 :: PixelRGBA8
color3 = hsva 11 0.5 0.92 1.0
color4 = hsva 0  0.7 0.64 1.0 

data Quad = Quad
  { quadA :: V2 Float
  , quadB :: V2 Float
  , quadC :: V2 Float
  , quadD :: V2 Float
  } deriving (Eq)

fromIntegralVector :: V2 Int -> V2 Float
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

closedPath :: [V2 Float] -> [Line]
closedPath [] = []
closedPath [p] = [Line p p]
closedPath qs@(p1:p2:ps) = zipWith Line qs (p2:ps <> [p1])

quad :: Quad -> [Line]
quad (Quad a b c d) = closedPath [a, b, c, d]

quadAddNoise :: Quad -> Generate Quad
quadAddNoise (Quad a b c d) = do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 12 --changed from 5 to 0
    perlinScale = 0.9 --changed from 0.1 to 0
    perlinPersistance = 0.9 --changed from 0.5 to 0
    perlinNoise
      = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y)
      = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure $ Quad
    (realToFrac <$> addNoise (realToFrac <$> a))
    (realToFrac <$> addNoise (realToFrac <$> b))
    (realToFrac <$> addNoise (realToFrac <$> c))
    (realToFrac <$> addNoise (realToFrac <$> d))

sketchQuad :: Double -> Double -> [Line] -> Generate (Drawing PixelRGBA8 ())
sketchQuad c3 c4 ls = do
  stroked <- weighted [(True, 0.6), (False, 0.4)]
  color <- uniform
    [ (color2 c3)
    , color3
    , color4
    , (color1 c4)
    ]
  return $ withTexture (uniformTexture color)
         $ if stroked
             then stroke 0.15 (JoinMiter 0) (CapStraight 0, CapStraight 0) ls
             else fill ls


genQuadGrid :: Float -> Float -> Int -> Generate [Quad]
genQuadGrid x y numberOfQuads  = do
  (w, h) <- getSize @Int
  vectors <- replicateM numberOfQuads $ do -- 800 replaced with y
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Quad v' (v' ^+^ V2 0 1.5) (v' ^+^ V2 1.5 x) (v' ^+^ V2 y 0) -- 1.5 replaced with x

sketch :: Double -> Double -> Float -> Float -> Int -> Generate (Drawing PixelRGBA8 ())
sketch c3 c4 x y numberOfQuads = do
  quads      <- genQuadGrid x y numberOfQuads
  noisyQuads <- traverse quadAddNoise quads
  mconcat <$> traverse (sketchQuad (c3) (c4) . quad) noisyQuads

