{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Set as Set
import System.Random
import Data.List
import Text.PrettyPrint
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Data.List
import qualified Debug.Trace as Trace
import Maybe
import System( getArgs )
import System.IO
import Game.ConnectkPrime
import Game.Game
import MCTS.Mcts
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import System.Process
import System.Exit
import Data.Array.Unboxed
playerList0 = B:B:A:A:playerList0


main = do 
        masterGen <- newStdGen
        rawList <- readProcess "cat" ["/tmp/pipe1"] []
	putStrLn rawList
        let (z',list) = (read rawList) :: (Int,[[Maybe (Player Connectk')]])
        let playerList = (replicate z' A)  ++ playerList0
        let k = 5
        let n = length list
        let arr = listArray' ((1,1),(n,n)) $ concat list
        let (noteToSelfWin,noteToSelfSave) = runST $ do
                                                      arr' <- unsafeThaw arr
                                                      a <- winningMoves z' arr' A k
                                                      b <- winningMoves z arr' B k
                                                      return (a,b)
        hPrint stderr $ show (noteToSelfWin,noteToSelfSave)
        let doTree a = do
                        let tree = iterativeMcts (expand $ gameToLeaf $ Connectk' k arr playerList (a, [])) 10000 masterGen
                        hPrint stderr tree
                        let (Connectk' _ board _ _) = last $ selectBestMoves 1 tree
                        return (diffs board arr)
        moves <- calculateMoves z' (noteToSelfWin,noteToSelfSave) doTree
        forM moves $ \(x,y)-> do
          write x y
  where
    write x y = do
      handle<-runCommand $ "echo \"" ++ (show (x-1)) ++ " " ++ (show (y-1)) ++ "\">/tmp/pipe2"
      result<-waitForProcess handle
      if (result/=ExitSuccess) then 
        putStrLn "oops error"
      else
        putStrLn $ "successfully wrote" ++ show x ++ " " ++ show y

    calculateMoves z' (x:xs,_) doTree = return x
    calculateMoves z' ([],noteToSelfSave) doTree | null noteToSelfSave = doTree [[]]
                                              | null noteToSelf' = do
                                                                    hPrint stderr $ show $ take z' $ head noteToSelf
                                                                    return $ take z' $ head noteToSelf
                                              | otherwise = doTree noteToSelf'
     where noteToSelf = settify $ map (sort.settify) $ choose' noteToSelfSave
           noteToSelf' = filter ((<=z').length) noteToSelf

      
diffs board1 board2 = map (fst . fst) $ filter (\(a,b) -> (a/=b)) $ zip (assocs' board1) $ assocs' board2


mainFun xxx = game
  where raw = [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Just A,Nothing,Just A,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Just B,Just A,Nothing,Nothing,Nothing],[Nothing,Nothing,Just A,Just B,Just A, Just B,Just A,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Just B,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just B]]
        zzz = listArray' ((1,1),(9,9)) $ concat raw
        game = Connectk' 5 zzz (playerList) (filter ((<=z).length) $ settify $ map (settify.sort) $ choose' noteToSelf,[])
        noteToSelf = runST $ do
          arr<-unsafeThaw zzz
          winningMoves 2 arr A 5 
        tree = iterativeMcts (expand $ gameToLeaf $ game) xxx $ mkStdGen 3238
        playerList=B:B:A:A:playerList


mainFun2 xxx= simulate (Connectk' k board (p:ps) (noteToSelf,noteToNext)) $ mkStdGen xxx
--simulate'' (Connectk' k board (p:ps) (noteToSelf,noteToNext)) $ mkStdGen xxx
  where board :: UArray (Int,Int) Int
        board = array ((1,1),(9,9)) [((1,1),-1),((1,2),-1),((1,3),-1),((1,4),-1),((1,5),-1),((1,6),-1),((1,7),-1),((1,8),-1),((1,9),-1),((2,1),-1),((2,2),-1),((2,3),-1),((2,4),-1),((2,5),-1),((2,6),-1),((2,7),0),((2,8),0),((2,9),0),((3,1),-1),((3,2),-1),((3,3),-1),((3,4),-1),((3,5),-1),((3,6),-1),((3,7),-1),((3,8),1),((3,9),-1),((4,1),-1),((4,2),-1),((4,3),-1),((4,4),-1),((4,5),-1),((4,6),1),((4,7),0),((4,8),0),((4,9),0),((5,1),-1),((5,2),-1),((5,3),-1),((5,4),0),((5,5),1),((5,6),1),((5,7),1),((5,8),1),((5,9),-1),((6,1),-1),((6,2),-1),((6,3),-1),((6,4),-1),((6,5),1),((6,6),-1),((6,7),-1),((6,8),-1),((6,9),0),((7,1),-1),((7,2),-1),((7,3),-1),((7,4),1),((7,5),-1),((7,6),1),((7,7),-1),((7,8),-1),((7,9),0),((8,1),-1),((8,2),0),((8,3),-1),((8,4),-1),((8,5),-1),((8,6),-1),((8,7),1),((8,8),-1),((8,9),1),((9,1),1),((9,2),0),((9,3),-1),((9,4),-1),((9,5),-1),((9,6),-1),((9,7),-1),((9,8),-1),((9,9),-1)] 

        k=5
        noteToSelf =[[(5,9)]]
        noteToNext =[]
        playerList=A:B:playerList
	p=A
        ps=B:playerList
        checkMove :: Board' -> Move' -> [[Move']]
        checkMove arr move = runST $ do
          arr' <- unsafeThaw arr
          checkNewAddition 2 arr' move p k
        board' move = board?//?(move,Just p)
        noteToNext' move = (checkMove (board' move) move) ++ noteToNext
        moves = map fst $ filter (\a->isNothing $ snd a) $ assocs' board
        changeOverMoves' move | null $ noteToNext' move = [[]] 
                              | otherwise = filter (\x -> (length x)<=z) $ settify $ map (settify.sort) $ choose' $ noteToNext' move
        simulate'' :: Connectk' -> StdGen -> [Connectk']
        simulate'' x gen = case (currentState x) of InProgress -> let (x', gen') = doPseudoRandomMove gen x in x:simulate'' x' gen'
                                                    a          -> [x]

