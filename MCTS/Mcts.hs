{-# LANGUAGE TemplateHaskell #-}

module MCTS.Mcts where

import Text.PrettyPrint.HughesPJ
import System.Random
import Data.List
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Data.List
import qualified Debug.Trace as Trace
import Maybe
import Game.TicTacToe
import Game.Game

simulateNode :: (Game a, RandomGen g) => MCT a -> g -> (MCT a, GameState (Player a), g)
simulateNode t g = (doBackpropagation t r, r, g')
                       where 
                          (r, g') = simulate (rGame t) g

--pickSplit :: RandomGen g => g-> [a] -> (a, [a], g) 
--pickSplit _ [] = undefined
--pickSplit gen xs = (b, as++bs, gen')
--        where
--          (number, gen') = randomR (0,(length xs)-1) gen
--	  (as, (b:bs)) = splitAt number xs

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
                          q <- arbitrary
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


gameToLeaf :: Game a => a -> MCT a
gameToLeaf game = MCT game 0 (map (\x -> (x,0)) $ allPlayers game) []

expand :: Game a=> MCT a -> MCT a
expand t = t{rChildList=(map (expand . gameToLeaf) $ legalChildren $ rGame t)}

--topMcts :: (Game a, Eq a, Show a, RandomGen g) => MCT a -> g -> (MCT a, g)
--basically the same as the standard mcts functuon except it doesnt delete exxhausted nodes. exhaustred nodes may be the best choice at the top level and we wouldn't be able to detect that otherwise 

mcts :: (Game a, RandomGen g) => MCT a -> g -> (MCT a, GameState (Player a), g)
mcts t g | (currentState $ rGame t)/= InProgress = (doBackpropagation t (currentState $ rGame t), (currentState $ rGame t), g)
         | 0 == ( rPlayed t ) = simulateNode t g
         | otherwise = (doBackpropagation (t{rChildList=(t'':ts')}) resultState, resultState, gen')
             where
                  (t', ts', g') = doSelection (currentPlayer $ rGame t) (rChildList t) g
                  (t'', resultState, gen') = mcts t' g'


prop_mcts :: MCT (TicTacToe) -> StdGen -> Property --with prop_mcts we assume that there is no  
prop_mcts t g = seq (duplicates [t]) (comparison_logic t t' )
    where
       comparison_logic :: MCT TicTacToe -> MCT TicTacToe -> Property
       comparison_logic t t' = not (duplicates $ rChildList t) ==>
                               (prop_childlist t t') .&&. (prop_increments t t') 


       prop_childlist t t' | ((currentState $ rGame t) /= InProgress) =                printTestCase "Since game is InProgress, checking that childlist empty" $
                                                                                        (null $ rChildList t')     --No children 
                             
                           | ((rPlayed t) == 0) =                                      printTestCase "Since game tree has 0 play count, checking that childlists of testcase and output have same childlist (this is only a shallow check)" $
                                                                                        ((length $ rChildList t) == (length $ rChildList t')) &&    --This line and following line ensure
                                                                                        (gameListSubset (rChildList t) (rChildList t'))             --Child lists contain same games (shallow check) 
                           
                           | ((length $ rChildList t') == (length $ rChildList t)) =   case (gameListSubsetWithDiscrepency (rChildList t') (rChildList t)) of
                                                                                        Left a ->  a
                                                                                        Right a -> comparison_logic (Maybe.fromMaybe undefined $ getGameFromList a $ rChildList t) (Maybe.fromMaybe undefined $ getGameFromList a $ rChildList t') -- recursive step, can use undefined because we already know that there is that game there
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
       
       (t',_,_) = mcts t g

doSelection :: (Game a, RandomGen g) => Player a -> [MCT a] -> g -> (MCT a, [MCT a], g)
doSelection p ts g = let (a, b, c) = doComparisons pairs g in (fst a, map fst b, c)
  where
    doComparisons :: (Game a, RandomGen g) => [(MCT a, Double)] -> g -> ((MCT a, Double),[(MCT a, Double)], g)
    doComparisons [] g = error "doComparisons of empty list"
    doComparisons [x] g = (x,[], g)
    doComparisons (x:xs) g
      | q' == q  = if a then (x',x:xs',g'') else (x,xs,g'') 
      | q' > q = (x',x:xs',g'')
      | otherwise = (x,xs,g'')
        where (t,q) = x
              (t',q') = x'
              (a,g') = random g
              (x', xs', g'') = doComparisons xs g'
    
    --ucbScores :: Game a => [MCT a] -> Int -> [Double]
    ucbScores [] _ = []
    ucbScores (t:ts) n = averageScore t + s : (ucbScores ts n)
      where s = 1 * (sqrt ((log (fromIntegral n)) / (fromIntegral $ rPlayed t)))

    pairs = zip ts $ ucbScores ts $ sum $ map rPlayed ts
    
    --averageScore :: Game a => MCT a -> Double
    averageScore t | rPlayed t == 0 = 0
                   | otherwise = (readTuple p $ rQ t) / (fromIntegral (rPlayed t))

prop_doSelection :: [MCT TicTacToe]-> StdGen -> Property
prop_doSelection ts g = not (null ts) ==>
                        printTestCase "Checking doSelection produces a list of the correct length" $
                        (length ts') == (length ts) - 1
  where 
    (t', ts', g') = doSelection A ts g

update :: Eq p => p -> (Double -> Double) -> (Double -> Double) -> [(p,Double)] -> [(p,Double)]
update _ _ _ [] = [] 
update p f g ((p',n'):ps) = if (p==p') then (p',f n'):(update p f g ps) else (p',g n'):(update p f g ps)

readTuple :: Eq p => p -> [(p,Double)] -> Double
readTuple _ [] = error "tried to read a list with non element"
readTuple p ((p',n'):ps) = if (p==p') then n' else readTuple p ps

doBackpropagation :: Game a => MCT a -> GameState (Player a) -> MCT a --not recurive
doBackpropagation t s = let t' = t{rPlayed = (rPlayed t)+1} in
			case s of Win x -> t'{rQ = update x (+1) (+(-1)) (rQ t')}
				  Stale -> t'
				  InProgress -> t
				  --_ -> error "Encountered undefined state"


iterativeMcts :: (Game a, RandomGen g) => MCT a -> Int -> g -> MCT a
iterativeMcts t 0 g = t
iterativeMcts t n g = let (a, _,g') = mcts t g in iterativeMcts a (n-1) g'

selectBestMoves :: Game a => Int-> MCT a -> [a]
selectBestMoves 0 _ = []
selectBestMoves n t = (rGame best):(selectBestMoves (n-1) best)
  where best = foldl (\a->(\b->if (rPlayed a) > (rPlayed b) then a else b)) (head $ rChildList t) (rChildList t)



instance Arbitrary StdGen where
         arbitrary = do i<-arbitrary
                        return $ mkStdGen i
