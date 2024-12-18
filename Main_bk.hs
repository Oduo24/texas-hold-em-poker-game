-- Texas Hold'em Poker Game Implementation in Haskell

module Main where

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

data Strategy = RandomStrategy deriving (Show)

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
-- Detailed implementation of hand evaluation
data HandRank = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show, Eq, Ord)

-- HandRank Enum instance
instance Enum HandRank where
  fromEnum HighCard        = 1
  fromEnum OnePair         = 2
  fromEnum TwoPair         = 3
  fromEnum ThreeOfAKind    = 4
  fromEnum Straight        = 5
  fromEnum Flush           = 6
  fromEnum FullHouse       = 7
  fromEnum FourOfAKind     = 8
  fromEnum StraightFlush   = 9
  fromEnum RoyalFlush      = 10

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
isFlush :: [Card] -> Bool
isFlush cards = length (groupBy ((==) `on` snd) cards) == 1

isStraight :: [Card] -> Bool
isStraight cards = let ranks = sort $ map fst cards
                   in and $ zipWith (\a b -> fromEnum b - fromEnum a == 1) ranks (tail ranks)

countRanks :: [Card] -> [(Rank, Int)]
countRanks cards = map (\g -> (head g, length g)) . group . sort $ map fst cards

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
    rankCounts = countRanks cards

evaluateHand :: [Card] -> [Card] -> Int
evaluateHand holeCards communityCards = fromEnum $ getHandRank (holeCards ++ communityCards)

-- Determine Winner
determineWinner :: [Player] -> [Card] -> [Player]
determineWinner players communityCards =
  let playerScores = [(p, evaluateHand (hand p) communityCards) | p <- players]
  in map fst . head . groupBy ((==) `on` snd) . sortBy (comparing (negate . snd)) $ playerScores

-- Betting Round
bettingRound :: GameState -> IO GameState
bettingRound state = do
  updatedBets <- mapM randomBet (players state)
  let updatedPot = pot state + sum (map snd updatedBets)
  return state { bets = updatedBets, pot = updatedPot }
  where
    randomBet player = do
      bet <- randomRIO (0, chips player `div` 10)
      return (name player, bet)

-- Game Loop
gameLoop :: GameState -> Int -> IO Player
gameLoop state roundCount
  | length (players state) == 1 = return (head (players state))
  | roundCount > 100 = return (maximumBy (comparing chips) (players state))
  | otherwise = do
      putStrLn $ "Starting round " ++ show roundCount
      shuffledDeck <- shuffleDeck (deck state)
      let (holeCards, deckAfterDeal) = dealCards (2 * length (players state)) shuffledDeck
      let updatedPlayers = zipWith (\p cards -> p { hand = cards }) (players state) (chunksOf 2 holeCards)
      let stateWithPlayers = state { players = updatedPlayers, deck = deckAfterDeal }
      roundAfterBets <- bettingRound stateWithPlayers

      let (flop, deckAfterFlop) = dealCards 3 (deck roundAfterBets)
      let stateWithFlop = roundAfterBets { communityCards = flop, deck = deckAfterFlop }
      stateAfterFlopBets <- bettingRound stateWithFlop

      let (turn, deckAfterTurn) = dealCards 1 (deck stateAfterFlopBets)
      let stateWithTurn = stateAfterFlopBets { communityCards = communityCards stateAfterFlopBets ++ turn, deck = deckAfterTurn }
      stateAfterTurnBets <- bettingRound stateWithTurn

      let (river, deckAfterRiver) = dealCards 1 (deck stateAfterTurnBets)
      let stateWithRiver = stateAfterTurnBets { communityCards = communityCards stateAfterTurnBets ++ river, deck = deckAfterRiver }
      stateAfterRiverBets <- bettingRound stateWithRiver

      let winners = determineWinner (players stateAfterRiverBets) (communityCards stateAfterRiverBets)
      if length winners == 1
        then return (head winners)
        else gameLoop stateAfterRiverBets (roundCount + 1)

-- Main
-- Test example to shuffle and deal cards
main :: IO ()
main = do
  let deck = createDeck
  shuffledDeck <- shuffleDeck deck
  let players = [Player "Alice" [] 1000 False (return "Fold"), Player "Bob" [] 1000 True (return "Call")]
  let (newDeck, newPlayers, communityCards) = dealCards players shuffledDeck
  print newDeck
  print newPlayers
  print communityCards
