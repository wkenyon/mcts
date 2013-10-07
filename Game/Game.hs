module Game.Game where
import System.Random

class (Eq a, Eq (Player a)) => Game a where
	legalChildren :: a->[a]
	currentState :: a->GameState (Player a)
	doPseudoRandomMove :: RandomGen g => g -> a -> (a,g)
        
        data Player a :: *
        
        allPlayers :: a -> [Player a]

        currentPlayer :: a -> Player a

        simulate :: RandomGen g => a -> g -> (GameState (Player a),g)
        simulate x gen = case (currentState x) of InProgress -> let (x', gen') = doPseudoRandomMove gen x in simulate x' gen'
                                                  a          -> (a, gen)


data GameState p = Win p | Stale | InProgress deriving (Show,Eq)
 


pick :: RandomGen g => g-> [a] -> (a, g) 
pick _ [] = error "picking from empty list"
pick gen xs = let (number, gen') = randomR (0,(length xs)-1) gen in
	(xs!!number, gen')

pick' :: RandomGen g => g-> [a] -> ((a,[a]),g)
pick' _ [] = undefined
pick' gen xs = let (number, gen') = randomR (0,(length xs)-1) gen in
        (xs ~!!~ number,gen')

(~!!~) :: [a] -> Int-> (a,[a])
(x:xs) ~!!~ 0 = (x,xs)
(x:xs) ~!!~ n = (y, x:ys)
  where (y,ys) = (xs ~!!~ (n-1))

randomPermutation :: RandomGen g => g -> [a] -> ([a],g)
randomPermutation gen [] = ([],gen)
randomPermutation gen xs = (y:zs,gen'')
  where ((y,ys), gen') = pick' gen xs
        (zs,gen'') = randomPermutation gen' ys
        

logi :: Int -> Int
logi a = ceiling $ log $ (fromIntegral a)+1
