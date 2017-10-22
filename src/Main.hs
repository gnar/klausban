module Main where

import System.IO(hFlush, hSetEcho, stdin, stdout)
import Data.List(sort, delete)
import Control.Applicative((<$>))
import Text.Printf(printf)

type Coord = (Integer, Integer) 

data Input = KLeft | KRight | KUp | KDown | KReset deriving (Eq, Show)

data World = World { wPlayer
                   , wSize :: Coord
                   , wWalls 
                   , wCrates
                   , wSlots  :: [Coord]
                   , wSteps :: Integer
                   } deriving (Eq, Show)

emptyWorld = World { wPlayer = (0, 0)
                   , wSize = (0, 0)
                   , wWalls = []
                   , wCrates = []
                   , wSlots = []
                   , wSteps = 0
                   }

parseWorld :: [String] -> World
parseWorld level = consume elems emptyWorld { wSize = (sizeX, sizeY) }
  where
    elems = foldr (++) [] [ [((x,y),ch) | (x, ch) <- zip [0..] row] | (y, row) <- zip [0..] level ]
    sizeX = fromIntegral $ maximum $ map length level
    sizeY = fromIntegral $ length level
    consume [] world = world
    consume ((pos, ch) : elems) world = case ch of
      '#' -> consume elems world { wWalls = pos : (wWalls world) }
      'o' -> consume elems world { wCrates = pos : (wCrates world) }
      '.' -> consume elems world { wSlots = pos : (wSlots world) }
      '@' -> consume elems world { wPlayer = pos }
      ' ' -> consume elems world
      otherwise -> error "undefined tile"

isWall world pos  = pos `elem` wWalls world
isCrate world pos = pos `elem` wCrates world
isSlot world pos  = pos `elem` wSlots world

isWorldSolved :: World -> Bool
isWorldSolved world = sort (wCrates world) == sort (wSlots world)

renderWorld :: World -> [String]
renderWorld world = renderAllRows
  where
    renderAllRows = [ renderRow y | y <- [0 .. (snd $ wSize world) - 1] ]
    renderRow y = [ renderCell (x, y) | x <- [0 .. (fst $ wSize world) - 1] ]
    renderCell pos = case () of () | isWall world pos -> '#'
                                   | pos == wPlayer world -> '@'
                                   | isCrate world pos -> 'o'
                                   | isSlot world pos -> '.'
                                   | otherwise -> ' '

updateWorld :: World -> Input -> World
updateWorld world input = 
  
  case () of () | isPushWall -> world
                | isPushCrate -> if canMoveCrate then moveCrate $ movePlayer world else world
                | otherwise -> movePlayer world

  where
    pos   = wPlayer world
    pos'  = advance pos input
    pos'' = advance pos' input

    advance (x, y) input = case input of 
      KLeft  -> (x-1, y)
      KRight -> (x+1, y)
      KUp    -> (x, y-1)
      KDown  -> (x, y+1)

    isPushWall   = isWall world pos'
    isPushCrate  = isCrate world pos'
    canMoveCrate = not (isWall world pos'') && not (isCrate world pos'')

    movePlayer world = world { wPlayer = pos', wSteps = 1 + wSteps world }
    moveCrate  world = world { wCrates = pos'' : (delete pos' $ wCrates world) }

renderHud :: World -> String
renderHud world = "---- [ Steps " ++ (show steps) ++ " ] ----"
  where
    steps = wSteps world

--------

getPlayerInput :: IO Input
getPlayerInput = do
  ch <- getChar
  case ch of
    'w' -> return KUp
    'a' -> return KLeft
    's' -> return KDown
    'd' -> return KRight
    'r' -> return KReset
    otherwise -> getPlayerInput

loadWorld :: String -> IO World
loadWorld filename = parseWorld <$> lines <$> readFile filename

mainloop :: World -> IO ()
mainloop world = do
  putStrLn ""
  putStrLn $ unlines $ renderWorld world
  putStrLn $ renderHud world
  if isWorldSolved world
    then do
      putStrLn "Congratulations!"
      return ()
    else do
      input <- getPlayerInput
      if input == KReset
        then return ()
        else mainloop $ updateWorld world input

main :: IO ()
main = do 

  hSetEcho stdin True
  hSetEcho stdout True

  putStr "Level? " >> hFlush stdout
  l <- read <$> getLine
  let filename = printf "levels/level_%02d.txt" (l :: Integer)
  world <- loadWorld filename

  hSetEcho stdin False
  hSetEcho stdout False

  mainloop world
  main

