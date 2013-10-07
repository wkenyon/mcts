{-# LANGUAGE TemplateHaskell #-}
module Game.ConnectkPrime where

import System.Random
import Data.List
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import qualified Debug.Trace as Trace
import Maybe
import Game.Game
import Game.Connectk hiding (A,B,X)
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
runTests = $quickCheckAll
z=2
type Move' = (Int,Int)
type Board' = UArray Move' Int
type STBoard' s = STUArray s Move' Int
data Connectk' = Connectk' Int Board' [Player Connectk'] ([[Move']],[[Move']]) deriving Eq

instance Show Connectk' where
         show (Connectk' k board turns forced) = (show k) ++ " \n" ++ (showboard board 9) ++ " " ++ (show $ take 10 turns) 
                                                          ++ " " ++ (show forced) ++ "\n"
           where
             matricise [] _ = []
             matricise x n = (take n x) : matricise (drop n x) n

             showboard board n = map (\x -> if (x=='9') then '_' else x) $ concatMap (\x -> show x++"\n") $ matricise (map (\x -> if (x==(-1)) then 9 else x) $ elems board ) n

instance Game Connectk' where
        data Player Connectk' = A | B | X deriving (Eq,Enum,Show,Read)
        allPlayers _ = [A,B]
        currentPlayer (Connectk' _ _ turns _) = head turns
        
        legalChildren (Connectk' _ _ [] _) = error "legal children with no turns"
        legalChildren (Connectk' k board (p:ps) (noteToSelf,noteToNext))
          | null noteToSelf = error "tried to find legalchildren with null noteToSelf"
          | (not $ any null noteToSelf) && p==(head ps) = map (\(x,xs) -> Connectk' k (board' x) ps (xs,noteToNext' x)) $ mergeHead noteToSelf
          | (not $ any null noteToSelf) = map (\(x,xs) -> Connectk' k (board' x) ps (changeOverMoves' x,[])) $ mergeHead noteToSelf
          | p==(head ps) = map (\x -> Connectk' k (board' x) ps ([[]], noteToNext' x)) moves
          | otherwise = map (\x -> Connectk' k (board' x) ps (changeOverMoves' x,[])) moves
          where
            board' move = board?//?(move,Just p)
            noteToNext' move = (checkMove (board' move) move) ++ noteToNext
            moves = map fst $ filter (\a->isNothing $ snd a) $ assocs' board
            
            changeOverMoves' move | null $ noteToNext' move = [[]] 
                                  | otherwise = filter (\x -> (length x)<=z) $ settify $ map (settify.sort) $ choose' $ noteToNext' move
  
            checkMove :: Board' -> Move' -> [[Move']]
            checkMove arr move = runST $ do
              arr' <- unsafeThaw arr
              checkNewAddition z arr' move p k
 
        currentState g | not $ any isNothing $ elems' board = Stale 
                       | null noteToSelf = Win $ nextp ps
                       | otherwise = InProgress
                       where (Connectk' _ board ps (noteToSelf,noteToNext)) = g
        
        doPseudoRandomMove gen game = pick gen $ 
                                      legalChildren game
 
        simulate (Connectk' k board turns (noteToSelf, noteToNext)) gen = runST $ do
          board' <- thaw board
          doMoves board' (savMoves++moves) turns noteToNext k gen''
          where
            emptySquares = map fst $ filter (\a->isNothing $ snd a) $ assocs' board 
            (moves, gen') = randomPermutation gen emptySquares
            (savMoves, gen'') = pick gen' noteToSelf

nextp :: [Player Connectk'] -> Player Connectk'
nextp (p:ps) | p==(head ps) = nextp ps
             | otherwise = head ps

mergeHead :: Eq a => [[a]] -> [(a,[[a]])]
mergeHead xs = foldr f [] list
  where
    list = map (\(x:xs) -> (x,[xs])) xs
    f a [] = [a]
    f (b,bs) ((c,cs):css) | (b==c) = (b,bs++cs):css
                          | otherwise = (c,cs) : (f (b,bs) css)
choose' :: [[Move']] -> [[Move']]
choose' [] = [[]]
choose' (ms:mss) = do
                     a <- choose' mss
                     map (:a) ms

winningMoves :: Int -> (STUArray s (Int,Int) Int) -> Player Connectk' -> Int -> (ST s) [[(Int,Int)]]
winningMoves z arr player k = do
  bounds <- getBounds arr
  winners<-forM (range bounds) (\a -> do
    elem <- readArray' arr a
    if (Just player == elem) then 
      checkNewAddition z arr a player k
    else 
      return [])
  return $ settify (map sort (concat winners))

settify :: Eq a => [a]->[a]
settify [] = []
settify (x:xs) = x : (delete x $ settify xs)

prop_settify :: [Int] -> Bool
prop_settify = f.settify
  where f [] = True
        f (x:xs) = (prop_settify xs) && (not $ x `elem` xs)

doMoves :: RandomGen g => STBoard' s -> [Move'] -> [Player Connectk'] -> [[Move']] -> Int -> g -> (ST s) (GameState (Player Connectk'),g)
doMoves _ [] _ _ _ gen = return (Stale,gen)
doMoves arr (m:moves) (p:players) noteToNext k gen = do
  a<-readArray' arr m
  if (not $ isNothing a) then doMoves arr moves (p:players) noteToNext k gen
  else do
    writeArray' arr m (Just p)
    winners <- checkNewAddition z arr m p k
    let noteToNext' = winners ++ noteToNext
    if (p==(head players)) then
      doMoves arr moves players noteToNext' k gen
    else if (not $ null noteToNext') then do
      let w = filter (\x -> length x <= z) $ settify $ map settify $ choose' noteToNext'
      if (null w) then
        return (Win p, gen)
      else do
        let (ms, gen') = pick gen w 
        doMoves arr (ms++moves) players [] k gen'
    else doMoves arr moves players [] k gen

--note prop_doMoves test relies on legalChildren producing children in same order
--that emptySquares does - which is the case at the time of writing, but if this changes then this 
--test will fail.
{-
prop_doMoves :: Connectk -> Property
prop_doMoves ck = (currentState ck) == InProgress ==> --because MCTS only ever simulates InProgress games
                  arrayResults == (map (\a -> if (a==InProgress) then Stale else a) normalResults)
  where normalResults = map currentState $ legalChildren ck
        arrayResults = map doArrayStuff emptySquares
        doArrayStuff a = runST $ do
                           arr <- newListArray' arrRange (concat board)
                           doMoves arr [(head turns,a)] k []
        (Connectk k board turns) = ck
        n=length board
        arrRange=((1,1),(n,n))
        emptySquares = [fst x | x<-zip (range arrRange) (concat board), isNothing (snd x)]
-}

instance Enum a => (Enum (Maybe a)) where
     fromEnum Nothing = -1
     fromEnum (Just a) = fromEnum a
     toEnum (-1) = Nothing
     toEnum n = Just (toEnum n)
{-
prop_checkNewAddition :: Connectk -> Property 
prop_checkNewAddition ck = (currentState ck) == InProgress ==> --because MCTS only ever simulates InProgress games
                           arrayResults == map (\a -> (a==(Win player))) normalResults
  where normalResults = map currentState $ legalChildren ck
        arrayResults = map doArrayStuff emptySquares
        doArrayStuff a = runST $ do
                           arr <- newListArray' arrRange (concat board)
                           writeArray' arr a (Just player)
                           checkNewAddition arr a player k
        (Connectk k board turns) = ck
        player = head turns
        n=length board
        arrRange=((1,1),(n,n))
        emptySquares = [fst x | x<-zip (range arrRange) (concat board), isNothing (snd x)]
-}
            


(?++?) a b   = (liftM2 (++)) a b

checkNewAddition :: Int -> STBoard' s -> Move' -> Player Connectk' -> Int -> (ST s) [[Move']]
checkNewAddition z arr loc player k = checkLine locUpLeft ((2*k)-1) k []   ?++?
                                      (checkLine locUp ((2*k) -1) k []      ?++?
                                      (checkLine locUpRight ((2*k) -1) k [] ?++?
                                      checkLine locRight ((2*k)-1) k []))
                                      
 where
--   checkLine :: forall a. (Int-> (Int,Int)) -> Int -> Int -> [Int] -> a
   checkLine f n 0 []     = return [[]] --outright win found
   checkLine f n 0 (x:xs) = do          --a win was found but had to assume some Nothing use, keep searching
                              otherLines <- checkLine f n ((k-x)+n+1) xs
                              return $ (map (f.(\a->a-k)) (x:xs)):otherLines
   checkLine f 0 _ _      = return []   --we searched over all the squares and no win was found
   checkLine f n a xs     = do
                              result <- readArray' arr (f (n-k))
                              case result of Nothing -> if (z==0) then 
                                                          checkLine f (n-1) k []
                                                        else 
                                                          if ((length xs) == z) then
                                                            let (x':xs') = xs in checkLine f (n-1) ((k-x')+n) (xs'++[n])
                                                          else
                                                            checkLine f (n-1) (a-1) (xs++[n])
                                                        
                                             Just player' -> if player' == player then checkLine f (n-1) (a-1) xs
                                                                                  else checkLine f (n-1) k []
   (y,x)=loc
   locUpLeft n  = (y-n,x-n)
   locUp n      = (y-n,x)
   locUpRight n = (y-n,x+n)
   locRight n   = (y,x+n)


{-
prop_Array'_funs :: Connectk -> Bool
prop_Array'_funs ck = --(currentState ck) == InProgress ==> --because MCTS only ever simulates InProgress games
                           arrayResults == normalResults
  where normalResults = map (concat . getBoard) $ legalChildren ck
        arrayResults = map doArrayStuff emptySquares
        doArrayStuff a = runST $ do
                           arr <- newListArray' arrRange (concat board)
                           writeArray' arr a (Just player)
                           arrBounds <- getBounds arr
                           mapM (\a -> readArray' arr a) $ range arrBounds
        (Connectk k board turns) = ck
        player = head turns
        n=length board
        arrRange=((1,1),(n,n))
        emptySquares = [fst x | x<-zip (range arrRange) (concat board), isNothing (snd x)]
        getBoard (Connectk _ board _) = board
-}
-- readArray' does the conversion out of enum & returns a blocking player X for out of bounds
readArray' :: STBoard' s -> Move' -> ST s (Maybe (Player Connectk'))
readArray' arr loc = do
		       bounds <- getBounds arr
                       if inRange bounds loc then
                         do 
                           result <- readArray arr loc
                           return $ toEnum result
                       else
                           return (Just (X::(Player Connectk')))

-- writeArray' does the conversion to enum, if out of bounds then writeArray deals with it
writeArray' :: STBoard' s -> Move' -> Maybe (Player Connectk') -> (ST s) ()
writeArray' arr loc val = writeArray arr loc (fromEnum val)

--newListArray' just does the same old enum conversion
newListArray' :: ((Int,Int),(Int,Int)) -> [Maybe (Player Connectk')] -> ST s (STBoard' s)
newListArray' range list = newListArray range $ map fromEnum list


      
listArray' :: ((Int,Int),(Int,Int)) -> [Maybe (Player Connectk')] -> Board'
listArray' a b = listArray a (map fromEnum b) 

assocs' :: Board' -> [(Move', Maybe (Player Connectk'))]
assocs' = (map (\(a,b)->(a,toEnum b))) . assocs

elems' :: Board' -> [Maybe (Player Connectk')]
elems' = (map toEnum) . elems

(?//?) :: Board' -> (Move', Maybe (Player Connectk')) -> Board'
(?//?) a (b,c) = a // [(b,fromEnum c)]
