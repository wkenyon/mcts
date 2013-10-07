{-# LANGUAGE TemplateHaskell #-}
module Game.TicTacToe where

import qualified Data.Set as Set
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
import Game.Game
        
runTests = $quickCheckAll

data TicTacToe = TicTacToe (Set.Set Int) (Set.Set Int) (Set.Set Int) deriving (Show,Eq)
instance Arbitrary TicTacToe where
         arbitrary = do (a,b,c) <- split3 [1..9]
                        return $ TicTacToe (Set.fromList a) (Set.fromList b) (Set.fromList c)
           where 
             split3 :: [a] -> QuickCheck.Gen ([a],[a],[a])
             split3 [] = return ([],[],[])
             split3 (x:xs) = do (a,b,c) <- split3 xs
                                QuickCheck.oneof [return (x:a,b,c), return (a,x:b,c), return (a,b,x:c)]

instance Game TicTacToe where
	data Player TicTacToe = A | B deriving (Show,Eq)
	legalChildren (TicTacToe x y z) = if ((Set.size y) == (Set.size z)) 
		then (map (\a -> TicTacToe (Set.delete a x) (Set.insert a y) z)  (Set.toList x))
		else (map (\a -> TicTacToe (Set.delete a x) y (Set.insert a z))  (Set.toList x))

        allPlayers _ = [A,B]
	
        currentPlayer (TicTacToe x y z) = if ((Set.size y) == (Set.size z)) then A else B

	currentState (TicTacToe x y z) 
 		| f $ Set.toList y = Win A 
		| f $ Set.toList z = Win B
		| null $ legalChildren $ TicTacToe x y z = Stale --TODO: aliasing here
		| otherwise = InProgress
                where 
                  f a = if (length a) < 3 then False else 15 `elem` (map sum $ choose 3 a)
	          
        doPseudoRandomMove gen board = pick gen (legalChildren board)

instance Arbitrary (Player TicTacToe) where
  arbitrary = QuickCheck.oneof [return A, return B]

choose :: Int -> [a] -> [[a]]  --Produce all the combinations of n elements from a list
choose 0 _ = [[]]
choose _ [] = undefined        --Undefined where n greater than the length of the list
choose n (x:xs) = (map (x:) $ choose (n-1) xs) ++ next
  where next = if n > (length xs) then [] else choose n xs

prop_choose :: LogarithmiclyIncreasingInt -> LogarithmiclyIncreasingList Int -> Property  --Define a property which should hold of the choose function
prop_choose n xs = n' <= length xs' ==>                                                   --Skip test cases where the length of the list shorter than n
                   (prop_len n' xs') .&&. (prop_memb n' xs')
  where 
   prop_len :: Int -> [a] -> Property
   prop_len n xs = printTestCase "Checking outer list has length `n choose k` where n is length of input list and k is the number of elements we are choosing" $ 
              ((fromIntegral $ length xs) `chooseN` (fromIntegral n)) == fromIntegral (length $ choose n xs)
   
   prop_memb :: Int -> [a] -> Property
   prop_memb n xs = printTestCase "Checking inner lists all have a length that is equal to the number of elements we are choosing" $ 
               foldl (\x-> \y-> x && (n==y)) True (map length (choose n xs)) 
   
   LII n' = n
   LIL xs' = xs

   chooseN :: Integer -> Integer -> Integer
   chooseN n k | n>=k = (fact n) `div` ((fact k)*(fact $ n-k))
               | otherwise = undefined 
 
   fact :: Integer->Integer
   fact 0 = 1
   fact 1 = 1
   fact x | x>1 = x*(fact (x-1))
          | otherwise = undefined

newtype LogarithmiclyIncreasingInt = LII Int
newtype LogarithmiclyIncreasingList a = LIL [a]

instance Arbitrary LogarithmiclyIncreasingInt where
         arbitrary = do i<-QuickCheck.sized (\a -> QuickCheck.choose(0,2 * logi a))
                        return $ LII i

instance Arbitrary a => Arbitrary (LogarithmiclyIncreasingList a) where
         arbitrary = do i<-QuickCheck.sized (\a -> QuickCheck.resize (3 * logi a) $ QuickCheck.listOf arbitrary)
                        return $ LIL i

instance Show LogarithmiclyIncreasingInt where
         show (LII i) = show i

instance Show a => Show (LogarithmiclyIncreasingList a) where
         show (LIL l) = show l

startBoard::TicTacToe
startBoard = TicTacToe (Set.fromList [1..9]) (Set.empty) (Set.empty)
