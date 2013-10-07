{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module MCTS.Config where
import MCTS.Game
import MCTS.Sample.TicTacToe
import Test.QuickCheck.Property
import System.Random
import Data.List
{-|
  This is the default configuration object.
  Use this if you want to use /UCB/ with /c/=1 for /Selection/ and 
  standard /Expansion/, /Simulation/ and /Backpropagation/. To make changes
  to the /c/ used for /UCB/ you should use the following:

  @
  'defaultConfig'{'doSelection'='doUcb' c}
  @

  To make other changes you probably want to import
  @'MCTS.Config'@ and do a similar trick with the helper functions there.
-}

defaultConfig :: MctsConfig
defaultConfig = MctsConfig 1 (doUcb 1) $
                \s -> case s of InProgress -> id
                                Stale -> (\(a,b) -> (a,b+1))
                                Win p -> (\(a,b) -> (update (==p) (+1) (+(-1)) a, b+1))

                                
{-|
  This does /UCB/ with a specific value of /c/.
-}
doUcb :: (Game a, RandomGen g) => Double -> [(ScoreTuple (Player a), Plays)] -> Player a -> g -> (Int,g)
doUcb c list p g | null list = error "can't do ucb from empty list"
                 | null played = (snd randomUnplayed, g')
                 | (ansscore > csqrtlogN) || (null unplayed) = (ans, g)
                 | otherwise = (snd randomUnplayed, g')
  where
    (unplayed,played) = partition ((==0).snd.fst) $ zip list [0..]
    (randomUnplayed,g') = pick g unplayed 
    (ansscore, ans) = {-# SCC "maxlist" #-} maximum $ map (\(a,b)->(ucbScore a,b)) played

    csqrtlogN = {-# SCC "sqrtlogbigN" #-} (*) c $ sqrt $ log $ fromIntegral $ sum $ map snd list --number of plays parent has
    ucbScore (st, n) = {-# SCC "ucbScore" #-} averageScore (st, n) + (csqrtlogN / (fromIntegral (n+1)))
--    averageScore :: (ScoreTuple (Player a), Plays) -> Double
    averageScore (st, 0) = 0
    averageScore (st, n) = (readTuple p st) / (fromIntegral n)
    
--prop_doUcb :: [(Double,Double,Plays)] -> Bool
--prop_doUcb game list = 
--  where 
--    all :: Player TicTacToe
--    all = allChildren



{-|
  Dictionary type containing configuration functions and constants for
  /Expansion/, /Selection/ and /Backpropagation/ phases of /MCTS/
-}
data MctsConfig = MctsConfig {
  {-|
    This constant specifies how many nodes to add to the tree on each simulation.
    If you can spare the increased memory usage, it may be advantagous to increase this number.
    It is not possible to only add nodes to the tree which have been simulated at least /n/ times
    due to the way the search is implemented. It might be desirable to do this to save memory usage 
  -}
  expandConst :: Int ,
  {-|
    Must select randomly among equal options
  -}
  doSelection :: (Game a, RandomGen g) => [(ScoreTuple (Player a),Plays)] -> Player a -> g -> (Int, g),
  {-|
    Not recursive
  -}
  doBackpropagation :: Game a => GameState (Player a)
                              -> (ScoreTuple (Player a), Plays)
                              -> (ScoreTuple (Player a), Plays)
}

update :: (p->Bool) -> (Score -> Score) -> (Score -> Score) -> ScoreTuple p -> ScoreTuple p
update b f g ps = map (\(p,q)->if b p then (p, f q) else (p,g q)) ps

prop_update :: Score -> Player TicTacToe -> Player TicTacToe ->Property
prop_update score playera playerb = playera /= playerb ==>
                                    (a'==(a+1)) && (b'==(b-1))
  where 
    a = readTuple playera bigQ
    b = readTuple playerb bigQ
    a' = readTuple playera bigQ'
    b' = readTuple playerb bigQ'
    bigQ' = update (==playera) (+1) (+(-1)) bigQ 
    bigQ = zip players [score, -score]
    players = allPlayers :: [Player TicTacToe]


readTuple :: Eq p => p -> ScoreTuple p -> Score
readTuple _ [] = error "tried to read a list with non element"
readTuple p ((p',n'):ps) = if (p==p') then n' else readTuple p ps

type ScoreTuple a = [(a,Score)]
type Score = Double
type Plays = Int
