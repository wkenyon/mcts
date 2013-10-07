{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module MCTS.Sample.Connectk where
import System.Random
import Data.List
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import qualified Debug.Trace as Trace
import Data.Maybe
import MCTS.Game
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

runTests = $quickCheckAll


type Board = [Row]
type Row = [Maybe (Player Connectk)]
type Turns = [Player Connectk]
data Connectk = Connectk Int Board Turns deriving Eq


instance Arbitrary Connectk where
         arbitrary = do n <- QuickCheck.sized (\a -> return $ max 1 $ min 19 $ (logi a)*3)
                        k <- QuickCheck.sized (\a -> return $ max 1 $ min n $ logi a)
                        m <- QuickCheck.sized (\a -> return $ max 1 $ min (n^2) $ ((logi a)*2)^2)
			i <- arbitrary
                        return $ doMoves m (emptyBoard n k) $ mkStdGen i
           where 
             doMoves :: Int -> Connectk -> StdGen -> Connectk
             doMoves 0 x _ = x
             doMoves m x gen = case (currentState x) of InProgress -> let (x', gen') = pick gen $ legalChildren x in doMoves (m-1) x' gen'
                                                        _          -> x
             playerList = A:B:playerList :: [Player Connectk]
             logi a = ceiling $ log $ (fromIntegral a)+1
             emptyBoard n k = Connectk k board playerList
               where 
                 board = replicate n (replicate n Nothing)
                        
prop_showConnectk :: Connectk -> Bool                        
prop_showConnectk x = currentState x == InProgress
                         
instance Show Connectk where
         show (Connectk k board turns) = (show k) ++ " " ++ (show board) ++ " " ++ (show $ take 10 turns)


instance Game Connectk where
        data Player Connectk = A | B | X deriving (Show,Eq,Read)
        allPlayers = [A,B]
        currentPlayer (Connectk _ _ turns) = head turns
        legalChildren (Connectk k board []) = undefined
        legalChildren (Connectk k board (p:ps)) = map (\a -> Connectk k a ps) (boardFillAllBlanks board)
          where
            boardFillAllBlanks :: Board -> [Board]
            boardFillAllBlanks [] = []
            boardFillAllBlanks (a:xs) = (map (:xs) (lineFillAllBlanks a)) ++ (map (a:)(boardFillAllBlanks xs))
            --append is not inneficient here because we're only preappending the number of elements on the row each time. 
            lineFillAllBlanks :: Row -> [Row]
            lineFillAllBlanks [] = [] 
            lineFillAllBlanks (a:xs) = case a of Nothing -> ((Just p):xs) : map (a:) (lineFillAllBlanks xs)
                                                 Just _  -> map (a:) (lineFillAllBlanks xs)

        currentState s = case winner of (Just x) -> Win x
                                        Nothing -> if (null $ legalChildren s) then Stale else InProgress

          where winner = firstMatch $ map (kInARow k) [board, transpose $ kDiag $ board, transpose $ kDiag $ map reverse board, transpose board, transpose $ kDiag $ transpose $ board,  transpose $ kDiag $ reverse $ board]
                Connectk k board turns = s

--        doSimulation (Connectk k board turns) gen = runST $ do
--          let board' = thaw board
--          doMoves board' (savMoves++moves) turns noteToNext k gen''
--          where
--            emptySquares = map fst $ filter (\a->isNothing $ snd a) $ assocs' board
--            (moves, gen') = randomPermutation gen emptySquares
--            (savMoves, gen'') = pick gen' noteToSelf
        
kInARow :: Int -> Board -> Maybe (Player Connectk)
kInARow k x = firstMatch $ map (\a->row a Nothing) x
  where
    row :: Row -> Maybe (Player Connectk,Int) -> Maybe (Player Connectk)
    row _ (Just (x, 0)) = Just x
    row [] _ = Nothing
    row (Nothing:xs) _ = row xs Nothing
    row ((Just x):xs) Nothing = row xs (Just (x,k-1))
    row ((Just x):xs) (Just (y,n)) | x==y = row xs (Just (x,n-1))
                                   | otherwise = row xs (Just (x, k-1))

firstMatch :: [Maybe (Player Connectk)] -> Maybe (Player Connectk)
firstMatch [] = Nothing
firstMatch (Nothing:xs) = firstMatch xs
firstMatch ((Just x):_) = Just x

kDiag a = map (\(a,b) -> a b) $ zip funs a
  where
    funs = id : (map (tail.) funs)
