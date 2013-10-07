{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module MCTS.Core (doIterativeMcts, 
                  doTimedMcts, 
                  mcts, 
                  expand, 
                  gameToLeaf,
                  selectBestMove,
                  MCT) where

import Text.PrettyPrint.HughesPJ
import System.Random
import Data.Time.Clock
import Data.List
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Data.List
import qualified Debug.Trace as Trace
import Data.Maybe
import MCTS.Sample.TicTacToe
import MCTS.Game
import MCTS.Config

data MCT a = MCT {rGame::a
                , rPlayed::Int
                , rQ::[(Player a,Double)]
                , rChildList::[MCT a]}

instance Arbitrary (MCT TicTacToe) where
         arbitrary = QuickCheck.sized $ \n->mkMCT $ ceiling $ log $ (fromIntegral n)+1 
           where
             mkMCT :: Int -> QuickCheck.Gen (MCT TicTacToe)
             mkMCT 0 = do game <- arbitrary
                          return $ expand $ gameToLeaf $ game --will automatically produce no children if stale. guaranteed to produce children if not stale so we avoid the nasty doSelection [] bug.
             mkMCT d = do game <- arbitrary                   --have to check for staleness
                          (QuickCheck.Positive plays)  <- arbitrary --never zero due to use of positive
                          score <- arbitrary
                          let q = zip allPlayers [score, -score]
                          diff <- QuickCheck.choose (1,1 `max` d)
                          d' <- return $ d-diff
                          children <- if ((currentState game) == Stale) then (return []) else QuickCheck.listOf1 $ mkMCT d' --checking for staleness. listOf1 because if it's not stale then we don't want to produce any games with no children, they would have been deleted otherwise.
                          return $ MCT game plays q children

instance (Game a, Show a, Show (Player a)) => Show (MCT a) where
                                  show = (show . (doc 2))
                                   where
                                    doc 0 _ = empty
                                    doc n t | (rPlayed t == 0) = (text $ show $ rGame t) <+> (text $ show $ currentState $ rGame t) <+> (text $ "len:") <+> (text $ show $ length $ rChildList t)
                                            | otherwise = (text $ show $ rGame t) <+> (text $ show $ currentState $ rGame t) <+> (int $ rPlayed $ t) <+> (text $ show $ rQ $ t) <+> (text $ "len:") <+> (text $ show $ length $ rChildList t)
                                                        $$(nest 1 $ vcat $ map (doc $ n-1) $ rChildList t)

{-|
  Do a number of iterations of /MCTS/ and return the most desirable game position.
  For example:

  @
  let (myGame',myRandomSeed') = doIterativeMcts myGame 'defaultConfig' myRandomSeed n
  @

  where n is the number of iterations to do.
-}
doIterativeMcts :: (Game a, RandomGen g) => a -> MctsConfig -> Int -> g -> (a,g)
doIterativeMcts game conf n gen = (selectBestMove endTree, gen')
  where
    startTree = expand $ gameToLeaf game
    (endTree, gen') = f startTree n gen
--    f :: MCT a -> Int -> g
    f t 0 g = (t, g)
    f t n g = let (t', _, g') = mcts t conf g in f t' (n-1) g'
{-|
  Do as many possible iterations of /MCTS/ possible in the availiable time and return 
  the most desirable game position wrapped up in IO monad. Most common use would be 
  directly in the main method, for example:

  @
  main = do
           ...
           (myGame',myRandomSeed') <- doTimedMcts myGame 'defaultConfig' myRandomSeed n
           ...
  @

  where @n@ is the number of milliseconds to search for. Note, the search will take
  a few milliseconds longer than this value, so you will have to play with this value
  if you are under strict time constraints.
-}

doTimedMcts :: Game a => a -> MctsConfig -> StdGen -> Int -> IO (a,StdGen)
doTimedMcts game conf gen n = do
    currentTimestamp <- getCurrentTime
    let maxTimeStamp = addUTCTime (fromIntegral n / 1000) currentTimestamp
    let startTree = expand $ gameToLeaf game
    (endTree, gen') <- f startTree maxTimeStamp 0 gen
    return (selectBestMove endTree, gen')
    where
      f t n n' g = do
                  currentTimestamp<-getCurrentTime
                  if currentTimestamp < n then do
                    let (t', _, g') = mcts t conf g 
                    (((f $! t') $! n) $! (n'+1)) $! g'
                  else do
                    putStrLn $ show n'
                    return (t, g)


--selectBestMoves :: Game a => Int-> MCT a -> [a]
--selectBestMoves 0 _ = []
--selectBestMoves n t = (rGame best):(selectBestMoves (n-1) best)
--  where best = foldl1 (\a->(\b->if (rPlayed a) > (rPlayed b) then a else b)) (rChildList t)

selectBestMove :: Game a => MCT a -> a
selectBestMove t = rGame $ maximumBy (\a -> \b -> compare (rPlayed a) (rPlayed b)) $ rChildList t

gameToLeaf :: Game a => a -> MCT a
gameToLeaf game = MCT game 0 (map (\x -> (x,0)) $ allPlayers) []

expand :: Game a=> MCT a -> MCT a
expand t = t{rChildList=(map (expand . gameToLeaf) $ legalChildren $ rGame t)}

mcts :: (Game a, RandomGen g) => MCT a -> MctsConfig -> g -> (MCT a, GameState (Player a), g)
mcts t conf g | s /= InProgress = (backP t s, s, g)
              | 0 == (rPlayed t) = (backP t ssim, ssim, g')
              | otherwise = (backP t{rChildList=(st':sts)} ssel, ssel, g''')
                  where
                    (ssim, g') = doSimulation (rGame t) g
                   
                    selectionParams = map (\a->(rQ a, rPlayed a)) $ rChildList t
                    (selectionIndex, g'') = doSelection conf selectionParams p g
                    (st, sts) = rChildList t ~!!~ selectionIndex
                    (st', ssel, g''') = mcts st conf g''

                    s = currentState $ rGame t
                    p = currentPlayer $ rGame t
                    
                    backP t state = t{rQ = bigQ', rPlayed=n'}
                      where
                        (bigQ',n') = doBackpropagation conf state (rQ t, rPlayed t)


prop_mcts :: MCT (TicTacToe) -> StdGen -> Property --with prop_mcts we assume that there is no  
prop_mcts t g = seq (duplicates [t]) (comparison_logic t t' )
    where
       comparison_logic :: MCT TicTacToe -> MCT TicTacToe -> Property
       comparison_logic t t' = not (duplicates $ rChildList t) ==>
                               (prop_childlist t t') .&&. (prop_increments t t') 


       prop_childlist t t' | ((currentState $ rGame t) /= InProgress) =                printTestCase "Since game is InProgress, checking that childlist empty" $ True
--                                                                                        (null $ rChildList t')     --No children 
                             
                           | ((rPlayed t) == 0) =                                      printTestCase "Since game tree has 0 play count, checking that childlists of testcase and output have same childlist (this is only a shallow check)" $
                                                                                        ((length $ rChildList t) == (length $ rChildList t')) &&    --This line and following line ensure
                                                                                        (gameListSubset (rChildList t) (rChildList t'))             --Child lists contain same games (shallow check) 
                           
                           | ((length $ rChildList t') == (length $ rChildList t)) =   case (gameListSubsetWithDiscrepency (rChildList t') (rChildList t)) of
                                                                                        Left a ->  a
                                                                                        Right a -> comparison_logic (fromMaybe undefined $ getGameFromList a $ rChildList t) (fromMaybe undefined $ getGameFromList a $ rChildList t') -- recursive step, can use undefined because we already know that there is that game there
                           | ((length $ rChildList t') == (length $ rChildList t)-1) = printTestCase "Since a deletion has occured, checking no spurious nodes have come in" $
                                                                                        (gameListSubset (rChildList t') (rChildList t))
                           
                           | otherwise =                                               printTestCase "The child lists have got different lengths"False

       prop_increments t t' = printTestCase "Checking that rPlayed gets incremented at the top level" $ 
                              (rPlayed t') == ((rPlayed t)+1)
 
       gameListSubsetWithDiscrepency :: Game a => [MCT a] -> [MCT a] -> Either Property (MCT a)
       gameListSubsetWithDiscrepency [] _ = Left $ printTestCase "gameListSubsetWithDiscrepency run but no discrepency found" False
       gameListSubsetWithDiscrepency (b:bs) c = case (getGameFromList b c) of Nothing -> Left $ printTestCase "gameListSubsetWithDiscrepency run but was not a subset" False
                                                                              Just a -> if ((rPlayed a) == (rPlayed b)) 
                                                                                        then gameListSubsetWithDiscrepency bs c 
                                                                                        else (if gameListSubset bs c then Right b else Left $ printTestCase "gameListSubsetWithDiscrepency was run and discrepancy found but the rest was not a subset" False)
       
       duplicates ::Game a => [MCT a] -> Bool
       duplicates [] = False
       duplicates (b:bs) = case (getGameFromList b bs) of Nothing -> duplicates bs
                                                          Just _ -> True
       
       gameListSubset :: Game a => [MCT a] -> [MCT a] -> Bool
       gameListSubset [] _ = True -- [] is a subset of anything
       gameListSubset (b:bs) c = case (getGameFromList b c) of Nothing -> False
                                                               Just a -> if ((rPlayed a) == (rPlayed b)) then gameListSubset bs c else False
       getGameFromList :: Game a => MCT a -> [MCT a] -> Maybe (MCT a)
       getGameFromList a [] = Nothing
       getGameFromList a (b:bs) = if ((rGame a) == (rGame b)) then Just b else (getGameFromList a bs)
       
       (t',_,_) = mcts t defaultConfig g



{-
prop_doSelection :: [MCT TicTacToe]-> StdGen -> Property
prop_doSelection ts g = not (null ts) ==>
                        printTestCase "Checking doSelection produces a list of the correct length" $
                        (length ts') == (length ts) - 1
  where 
    (t', ts', g') = doSelection A ts g
-}

instance Arbitrary StdGen where
         arbitrary = do i<-arbitrary
                        return $ mkStdGen i
