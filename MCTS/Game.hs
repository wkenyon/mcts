{-# LANGUAGE TypeFamilies, FlexibleContexts  #-}
module MCTS.Game where
import System.Random
class (Eq a, Eq (Player a)) => Game a where
        {-| 
          This function is your implementation of the game rules
          It should specify all of the legal moves which can be made from any 
          legal position. Behavior for illegal positions may be undefined.
        -}
        legalChildren :: a->[a]

        {-|
          This should not result in a representation of the current game position
          but the state in the sense of weather or not the game has terminated, and if it
          has, in what state. See @'GameState'@ for more details.
        -}
     	currentState :: a->GameState (Player a)
        
        {-|
          You must define your own player type. For a simple two player game you might use

          > data 'Player' a = Black | White | Dud

          where @Dud@ is a commonly used trick to help in implementation.
          For example, in chess, your @readPlayerFromBoard@ function might return @Dud@ if
          an out of range position is requested. This allows a simple way to prevent pieces moving
          off the board without needing to do messy bounds checking everywhere.
        -}
        data Player a :: *
        
        {-|
          This is the function which always returns the list of players which will be represented in the game tree.
          Following from the above example you would use

          > allPlayers _ = [Black, White]

          since you don't want to maintain a score for @Dud@ in the game tree.
        -}
        allPlayers :: [Player a]
        
        {-|
          This should return the player whose move it is.
          If you are implementing a simultanious play game, then you should still
          consider one player to move at a time, but treat the game as having imperfect information.
        -}
        currentPlayer :: a -> Player a
        
        {-|
          A default implementation is provided for this function. 
          Don't override it unless you want to modify the /Simulation/ phase of /MCTS/
          You may want to override it if you can provide a more efficient implementation using context specific knowledge,
          want to implement early termination of simulations, need to do some probabilistic guessing about the state at the
          start of simulation or want to implement a different simulation policy to uniform random.
        -}
        doSimulation :: RandomGen g => a -> g -> (GameState (Player a),g)
        doSimulation x gen = case (currentState x) 
          of InProgress -> doSimulation x' gen'
             a          -> (a, gen)
            where
              (x', gen') = pick gen (legalChildren x)

{-| 
  This is not your representation of the current game position, 
  it is what the current position means as far as the search is concerned.
  The values are fairly self explanatory.
-}
data GameState a = InProgress
                 | Stale
                 | Win a deriving (Eq,Show)



pick :: RandomGen g => g-> [a] -> (a, g) 
pick _ [] = error "picking from empty list"
pick gen xs = let (number, gen') = randomR (0,(length xs)-1) gen in
	(xs!!number, gen')

randomPermutation :: RandomGen g => g -> [a] -> ([a],g)
randomPermutation gen [] = ([],gen)
randomPermutation gen xs = (y:zs,gen'')
  where ((y,ys), gen') = pick' gen xs
        (zs,gen'') = randomPermutation gen' ys

pick' :: RandomGen g => g-> [a] -> ((a,[a]),g)
pick' _ [] = undefined
pick' gen xs = let (number, gen') = randomR (0,(length xs)-1) gen in
        (xs ~!!~ number,gen')

(~!!~) :: [a] -> Int-> (a,[a])
(x:xs) ~!!~ 0 = (x,xs)
(x:xs) ~!!~ n = (y, x:ys)
  where (y,ys) = (xs ~!!~ (n-1))
