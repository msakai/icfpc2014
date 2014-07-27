module Sim where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.IORef

import qualified GHostCPU
import qualified LambdaManCPU
import Game

type Pos = (Int,Int)

data Sim
  = Sim
  { simUTC  :: IORef UTC
  , simGameStatus :: IORef GameStatus
  , simCurrentMap :: IOArray Pos Symbol -- empty, pill, power pill, fruit, wall だけからなるマップ

  , simNumGhostAIs :: Int

  , simLambda1 :: (LambdaManCPU.Machine, LambdaState)
  , simGhosts  :: Array Int (GHostCPU.Machine, GhostState)

  , simOrigMap :: Array Pos Symbol
  -- Maps have one and only one fruit location.
  , simFruitLocation :: Pos

  , simFrightModeDeactivateSchedule :: IORef UTC
  }

data GameStatus
  = GameOverWin
  | GameOverLoose
  | GameContinuing
  deriving (Eq, Ord, Show)

simLevel :: Sim -> Level
simLevel sim = computeLevel (x1+1, y1+1)
  where
    ((0,0),(x1,y1)) = bounds (simOrigMap sim)

data LambdaState
  = LambdaState
  { lambdaStartPos   :: Pos
  , lambdaCurrentPos :: IORef Pos
  , lambdaCurrentDir :: IORef Dir
  , lambdaLife       :: IORef Int
  , lambdaScore      :: IORef Integer
  , lambdaScheduled  :: IORef UTC
  }

data GhostState
  = GhostState
  { ghostIndex      :: !Int
  , ghostStartPos   :: Pos
  , ghostCurrentPos :: IORef Pos
  , ghostCurrentDir :: IORef Dir
  , ghostVitality   :: IORef Vitality
  , ghostScheduled  :: IORef UTC
  }

initSim :: Sim
initSim = undefined

runTick :: Sim -> IO ()
runTick sim = do
  now <- readIORef (simUTC sim)
  lambda1Eating <- newIORef False

  -- 1. All Lambda-Man and ghost moves scheduled for this tick take
  -- place.  The next move is also scheduled at this point.  (Note
  -- that Lambda-Man and the ghosts do not move every tick, only every
  -- few ticks; see the ticks section below.
  moveLambdaMan sim (simLambda1 sim)
  forM_ (elems (simGhosts sim)) $ \ghost -> do
    moveGhost sim ghost

  -- 2. Next, any actions (fright mode deactivating, fruit
  -- appearing/disappearing) take place.
  t <- readIORef (simFrightModeDeactivateSchedule sim)
  when (t==now) $ do
    forM_ (elems (simGhosts sim)) $ \(cpu,ghost) -> do
      writeIORef (ghostVitality ghost) Standard
  when (now == utcFruit1Appear || now == utcFruit2Appear) $
    writeArray (simCurrentMap sim) (simFruitLocation sim) Fruit
  when (now == utcFruit1Expire || now == utcFruit2Expire) $
    writeArray (simCurrentMap sim) (simFruitLocation sim) Empty

  -- 3. Next, we check if Lambda-Man is occupying the same square as
  -- pills, power pills, or fruit:

  --   1. If Lambda-Man occupies a square with a pill, the pill is
  --   eaten by Lambda-Man and removed from the game.
  let lambda1 = simLambda1 sim
  pos <- readIORef (lambdaCurrentPos (snd lambda1))
  sym <- readArray (simCurrentMap sim) pos
  when (sym == Pill) $ do
    writeIORef lambda1Eating True
    modifyIORef' (lambdaScore (snd lambda1)) (+10)
    writeArray (simCurrentMap sim) pos Empty

  --   2. If Lambda-Man occupies a square with a power pill, the power
  --   pill is eaten by Lambda-Man, removed from the game, and fright
  --   mode is immediately activated, allowing Lambda-Man to eat
  --   ghosts.
  when (sym == PowerPill) $ do
    writeIORef lambda1Eating True
    modifyIORef' (lambdaScore (snd lambda1)) (+50)
    writeArray (simCurrentMap sim) pos Empty
    writeIORef (simFrightModeDeactivateSchedule sim) (now + frightModeDuration)
    forM_ (elems (simGhosts sim)) $ \(cpu,ghost) -> do
      writeIORef (ghostVitality ghost) FrightMode
      modifyIORef (ghostCurrentDir ghost) oppositeDir

  --   3. If Lambda-Man occupies a square with a fruit, the fruit is
  --   eaten by Lambda-Man, and removed from the game.
  when (sym == Fruit) $ do
    writeIORef lambda1Eating True
    modifyIORef' (lambdaScore (snd lambda1)) (+ fruitPoint (simLevel sim))
    writeArray (simCurrentMap sim) pos Empty

  -- 4. Next, if one or more visible ghosts are on the same square as
  -- Lambda-Man, then depending on whether or not fright mode is
  -- active, Lambda-Man either loses a life or eats the ghost(s).
  forM_ (elems (simGhosts sim)) $ \(cpu,ghost) -> do
    pos1 <- readIORef (ghostCurrentPos ghost)
    when (pos == pos1) $ do
      isFrightMode <- liftM (FrightMode==) $ readIORef (ghostVitality ghost)
      if isFrightMode then do
        {- While in fright mode, if a ghost occupies the same square
        as a Lambda-Man, the ghost is eaten. When a ghost is eaten, it
        is returned to its starting position and starting direction,
        and is invisible until fright mode expires. While invisible,
        the ghost can neither eat nor be eaten.
        -}
        writeIORef lambda1Eating True
        writeIORef (ghostCurrentPos ghost) (ghostStartPos ghost)
        writeIORef (ghostVitality ghost) Invisible
        writeIORef (ghostCurrentDir ghost) DirDown -- At the start of the game, all ghosts and Lambdaman face down.
      else do
        {- If at the end of a tick Lambda-Man is in the same square as
        a visible ghost and fright mode is not active then Lambda-Man
        loses a life. -}
        modifyIORef (lambdaLife (snd lambda1)) (subtract 1)
        {- In this case, Lambda-Man and all the ghosts are immediately
        returned to their starting positions and starting directions
        (so that at the beginning of the next tick, Lambda-Man and the
        ghosts are in their starting positions).  -}
        writeIORef (lambdaCurrentPos (snd lambda1)) (lambdaStartPos (snd lambda1))
        writeIORef (lambdaCurrentDir (snd lambda1)) DirDown -- At the start of the game, all ghosts and Lambdaman face down.
        forM_ (elems (simGhosts sim)) $ \(cpu,ghost) -> do
          writeIORef (ghostCurrentPos ghost) (ghostStartPos ghost)
          writeIORef (ghostCurrentDir ghost) DirDown -- At the start of the game, all ghosts and Lambdaman face down.

  -- 5. Next, if all the ordinary pills (ie not power pills) have been
  -- eaten, then Lambda-Man wins and the game is over.
  xs <- getElems (simCurrentMap sim)
  when (all (\x -> x /= Pill) xs) $ do
    writeIORef (simGameStatus sim) GameOverWin

  -- 6. Next, if the number of Lambda-Man lives is 0, then Lambda-Man loses and the game is over.
  life <- readIORef (lambdaLife (snd lambda1))
  when (life == 0) $ do
    writeIORef (simGameStatus sim) GameOverLoose

  b <- readIORef lambda1Eating
  t <- readIORef (lambdaScheduled (snd lambda1))
  when (t==now) $
    if b
    then writeIORef (lambdaScheduled (snd lambda1)) (now + 137)
    else writeIORef (lambdaScheduled (snd lambda1)) (now + 127)

  forM_ (elems (simGhosts sim)) $ \(cpu,ghost) -> do
    t <- readIORef (ghostScheduled ghost)
    when (t==now) $ do
      let ai = ghostIndex ghost `mod` simNumGhostAIs sim -- TODO: indexは0-originでよい?
          t1 = [130,132,134,136] !! ai
          t2 = [195,198,201,204] !! ai
      v <- readIORef (ghostVitality ghost)
      if v/=FrightMode
      then writeIORef (ghostScheduled ghost) (now+t1)
      else writeIORef (ghostScheduled ghost) (now+t2)

  -- 7. Finally, the tick counter is incremented.
  writeIORef (simUTC sim) (now+1)

moveLambdaMan :: Sim -> (LambdaManCPU.Machine, LambdaState) -> IO ()
moveLambdaMan sim (cpu,s) = do
  now <- readIORef (simUTC sim)
  t <- readIORef (lambdaScheduled s)
  when (t==now) $ do
    undefined

moveGhost :: Sim -> (GHostCPU.Machine, GhostState) -> IO ()
moveGhost sim (cpu,s) = do
  now <- readIORef (simUTC sim)
  t <- readIORef (ghostScheduled s)
  when (t==now) $ do
    undefined
