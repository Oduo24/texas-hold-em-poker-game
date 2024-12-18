-- Texas Hold'em Poker Game Implementation

-- Main imports
import System.Random (randomRIO)
import Data.List (delete, sortBy, groupBy, sort, maximumBy, group)
import Data.Ord (comparing)
import Data.Function (on)
import Data.List.Split (chunksOf)

-- Data Types
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum, Ord)

type Card = (Rank, Suit)
type Deck = [Card]

data Strategy = RandomStrategy | PassiveStrategy | AggressiveStrategy | SmartStrategy deriving (Show, Eq)

data Player = Player
  { name      :: String
  , hand      :: [Card]
  , chips     :: Int
  , isDealer  :: Bool
  , strategy  :: Strategy
  } deriving (Show)

data GameState = GameState
  { players        :: [Player]
  , deck           :: Deck
  , communityCards :: [Card]
  , pot            :: Int
  , bets           :: [(String, Int)] -- Player name and their bet
  , dealerPos      :: Int
  , smallBlindPos  :: Int
  , bigBlindPos    :: Int
  } deriving (Show)

-- Deck Handling

fullDeck :: Deck
fullDeck = [(rank, suit) | rank <- [Two .. Ace], suit <- [Hearts .. Spades]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return []
shuffleDeck d = do
  idx <- randomRIO (0, length d - 1)
  let picked = d !! idx
  rest <- shuffleDeck (delete picked d)
  return (picked : rest)

-- Dealing Cards
dealCards :: Int -> Deck -> ([Card], Deck)
dealCards n d = (take n d, drop n d)

-- Evaluating Hands
-- This section defines how the hands are ranked and evaluated.

data HandRank = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show, Eq, Ord)

-- Enum instance for HandRank to assign numerical values to each hand rank.
-- This is useful for comparing hand strengths.
instance Enum HandRank where
  fromEnum HighCard        = 1 -- The weakest hand rank
  fromEnum OnePair         = 2 
  fromEnum TwoPair         = 3 
  fromEnum ThreeOfAKind    = 4 
  fromEnum Straight        = 5 
  fromEnum Flush           = 6 
  fromEnum FullHouse       = 7 
  fromEnum FourOfAKind     = 8 
  fromEnum StraightFlush   = 9 
  fromEnum RoyalFlush      = 10 -- The strongest hand

  -- Mapping integers to HandRank.
  toEnum 1  = HighCard
  toEnum 2  = OnePair
  toEnum 3  = TwoPair
  toEnum 4  = ThreeOfAKind
  toEnum 5  = Straight
  toEnum 6  = Flush
  toEnum 7  = FullHouse
  toEnum 8  = FourOfAKind
  toEnum 9  = StraightFlush
  toEnum 10 = RoyalFlush
  toEnum _  = error "Invalid value for HandRank"

-- Helper Functions for Hand Evaluation

-- Checks if a hand is a flush.
isFlush :: [Card] -> Bool
isFlush cards = length (groupBy ((==) `on` snd) cards) == 1 -- Groups cards by suit only one group means all cards share the same suit.

-- Checks if a hand is a straight.
isStraight :: [Card] -> Bool
isStraight cards = 
  let ranks = sort $ map fst cards
  in and $ zipWith (\a b -> fromEnum b - fromEnum a == 1) ranks (tail ranks)

-- Counts the occurrences of each rank in the hand.
countRanks :: [Card] -> [(Rank, Int)]
countRanks cards = 
  map (\g -> (head g, length g)) -- Map each group to its rank and count.
  . group . sort $ map fst cards -- Group sorted ranks and count occurrences.

-- Determines the rank of a given hand
getHandRank :: [Card] -> HandRank
getHandRank cards
  | isFlush cards && isStraight cards && maximum (map fst cards) == Ace = RoyalFlush
  | isFlush cards && isStraight cards = StraightFlush
  | any ((== 4) . snd) rankCounts = FourOfAKind
  | any ((== 3) . snd) rankCounts && any ((== 2) . snd) rankCounts = FullHouse
  | isFlush cards = Flush
  | isStraight cards = Straight
  | any ((== 3) . snd) rankCounts = ThreeOfAKind
  | length (filter ((== 2) . snd) rankCounts) == 2 = TwoPair
  | any ((== 2) . snd) rankCounts = OnePair
  | otherwise = HighCard
  where
    rankCounts = countRanks cards -- Pre-computed rank counts for efficiency.

-- Evaluating a player's hand by combining hole cards and community cards.
evaluateHand :: [Card] -> [Card] -> Int
evaluateHand holeCards communityCards = 
  fromEnum $ getHandRank (holeCards ++ communityCards)

-- Determine Winner
-- This function identifies the winner among the players based on their hands.

determineWinner :: [Player] -> [Card] -> [Player]
determineWinner players communityCards =
  -- Evaluating each player's score by combining their hand with the community cards.
  let playerScores = [(p, evaluateHand (hand p) communityCards) | p <- players]
  in map fst
     . head
     . groupBy ((==) `on` snd)
     . sortBy (comparing (negate . snd)) $ playerScores


-- Logging
-- This function logs the details of the current game round to the console.

logRound :: GameState -> IO ()
logRound state = do
  putStrLn "\n--- Round Details ---"
  
  -- Log the current pot value.
  putStrLn $ "Pot: " ++ show (pot state)
  
  -- Log the bets made by each player during the round.
  putStrLn "Player Bets:"
  mapM_ (\(n, b) -> putStrLn $ n ++ " bet: " ++ show b) (bets state)

  -- Log the community cards on the table.
  putStrLn "Community Cards:"
  mapM_ print (communityCards state)

-- Strategies
-- This function applies a given strategy to determine how much a player will bet during their turn.

applyStrategy :: Strategy -> Player -> Int -> Int
applyStrategy RandomStrategy _ chips = chips `div` 10
-- RandomStrategy: Bets a random fraction (1/10) of the player's total chips.

applyStrategy PassiveStrategy _ _ = 0
-- PassiveStrategy: Always bets zero, indicating a conservative approach.

applyStrategy AggressiveStrategy _ chips = chips `div` 4
-- AggressiveStrategy: Bets a more significant fraction of the player's chips (1/4 of total).

applyStrategy SmartStrategy player chips =
  let handStrength = evaluateHand (hand player) [] in
  if handStrength > 6 
     then chips `div` 3 -- Bets 1/3rd of their chips if the hand strength is above a threshold of 6.
     else 0 -- eElse, does not bet at all.


bettingRound :: GameState -> IO GameState
bettingRound state = do
  -- Updating the bets for each player based on their strategies.
  updatedBets <- mapM (\player -> do
    -- Determinining the bet amount for the current player using their strategy.
    let bet = applyStrategy (strategy player) player (chips player)
    -- Return the player's name and their bet.
    return (name player, bet)) (players state)

  -- Calculates the new total pot by summing the bets of all players.
  let updatedPot = pot state + sum (map snd updatedBets)

  -- Return the updated game state
  return state { bets = updatedBets, pot = updatedPot }


-- Game loop
gameLoop :: GameState -> Int -> IO Player
gameLoop state roundCount
  -- If only one player remains, they are the winner.
  | length (players state) == 1 = return (head (players state))

  -- If the game exceeds 100 rounds, declare the player with the most chips as the winner.
  | roundCount > 100 = return (maximumBy (comparing chips) (players state))

  -- Recursively play a round of the game.
  | otherwise = do
      putStrLn $ "\nStarting round " ++ show roundCount

      -- Shuffle the deck and deal hole cards to players.
      shuffledDeck <- shuffleDeck (deck state)
      let (holeCards, deckAfterDeal) = dealCards (2 * length (players state)) shuffledDeck
      -- Assigning dealt cards to players and update the state.
      let updatedPlayers = zipWith (\p cards -> p { hand = cards }) (players state) (chunksOf 2 holeCards)
      let stateWithPlayers = state { players = updatedPlayers, deck = deckAfterDeal }

      -- Performing the first pre-flop round
      roundAfterBets <- bettingRound stateWithPlayers
      logRound roundAfterBets

      -- Deal the flop
      let (flop, deckAfterFlop) = dealCards 3 (deck roundAfterBets)
      let stateWithFlop = roundAfterBets { communityCards = flop, deck = deckAfterFlop }
      -- Perform a betting round.
      stateAfterFlopBets <- bettingRound stateWithFlop
      logRound stateAfterFlopBets

      -- Deal the turn
      let (turn, deckAfterTurn) = dealCards 1 (deck stateAfterFlopBets)
      let stateWithTurn = stateAfterFlopBets { communityCards = communityCards stateAfterFlopBets ++ turn, deck = deckAfterTurn }
      -- Perform a betting round.
      stateAfterTurnBets <- bettingRound stateWithTurn
      logRound stateAfterTurnBets

      -- Deal the river
      let (river, deckAfterRiver) = dealCards 1 (deck stateAfterTurnBets)
      let stateWithRiver = stateAfterTurnBets { communityCards = communityCards stateAfterTurnBets ++ river, deck = deckAfterRiver }
      -- Perform a betting round
      stateAfterRiverBets <- bettingRound stateWithRiver
      logRound stateAfterRiverBets

      -- Determine the winner based on the best hand.
      let winners = determineWinner (players stateAfterRiverBets) (communityCards stateAfterRiverBets)
      if length winners == 1
        then
          -- If there is a single winner, return them.
          return (head winners)
        else
          -- If there is a tie, continue the game.
          gameLoop stateAfterRiverBets (roundCount + 1)


-- Main
main :: IO ()
main = do
  shuffled <- shuffleDeck fullDeck
  let initialPlayers = [ Player { name = "Player " ++ show i, hand = [], chips = 10000, isDealer = i == 1, strategy = strat } 
                          | (i, strat) <- zip [1..4] [RandomStrategy, PassiveStrategy, AggressiveStrategy, SmartStrategy] ]
  let initialState = GameState { players = initialPlayers, deck = shuffled, communityCards = [], pot = 0, bets = [], dealerPos = 1, smallBlindPos = 2, bigBlindPos = 3 }
  winner <- gameLoop initialState 1
  putStrLn $ "\nWinner: " ++ name winner
