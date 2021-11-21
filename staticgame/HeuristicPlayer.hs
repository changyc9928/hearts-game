-- | Write a report describing your design and strategy here.
module HeuristicPlayer (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types
import System.Random
import Data.List

getSuit :: Card -> Suit
getSuit (Card s _) = s

getRank :: Card -> Rank
getRank (Card _ r) = r

haveSuit :: (Functor f, Foldable f) => f Card -> Suit -> Bool
haveSuit hand s = any (==s) $ getSuit <$> hand

haveCard :: [Card] -> Card -> Bool
haveCard hand card = if (find (==card) hand) /= Nothing then True else False

filterSuit :: [Card] -> Suit -> [Card]
filterSuit hand s = filter (\x -> if getSuit x == s then True else False) hand

getMemoryCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
getMemoryCards Nothing = []
getMemoryCards (Just (_, memory)) = (parseMemory memory)
  where
    parseMemory :: String -> [Card]
    parseMemory "" = []
    parseMemory string = map (\x -> Card (parseSuit $ x !! 0) (parseRank $ x !! 1)) (words string)

    parseSuit :: Char -> Suit
    parseSuit s 
      | s == 'S' = Spade
      | s == 'C' = Club
      | s == 'D' = Diamond
      | otherwise = Heart

    parseRank :: Char -> Rank
    parseRank s 
      | s =='A' = Ace
      | s =='K' = King
      | s =='Q' = Queen
      | s =='J' = Jack
      | s =='2' = Two
      | s =='3' = Three
      | s =='4' = Four
      | s =='5' = Five
      | s =='6' = Six
      | s =='7' = Seven
      | s =='8' = Eight
      | s =='9' = Nine
      | otherwise = Ten

getPreviousTricks :: Maybe ([(Card, PlayerId)], String) -> [Card]
getPreviousTricks Nothing = []
getPreviousTricks (Just (tricks, _)) = map fst tricks

updateMemory :: Maybe ([(Card, PlayerId)], String) -> String
updateMemory Nothing = ""
updateMemory (Just (tricks, memory)) = foldr (++) memory $ map (\(Card s r, _) -> mappend " " (show s ++ show r)) $ tricks

-- [(Card Spade Queen, "1"), (Card Spade Jack, "2"), (Card Heart Ace, "3")]

playCard :: PlayFunc
playCard _ hand [] previous = (lead hand previousCards, memory)
  where
    previousCards = getPreviousTricks previous ++ getMemoryCards previous
    memory = updateMemory previous
playCard _ hand tricks previous 
  | memoryCards == [] = (renege "bleed" leadCard hand currentCards previousCards, memory)
  | otherwise = (renege "nonBleed" leadCard hand currentCards previousCards, memory)
  where
    previousTricks = getPreviousTricks previous
    memoryCards = getMemoryCards previous
    leadCard = fst $ last tricks
    currentCards = map fst tricks
    memory = updateMemory previous
    previousCards = previousTricks ++ memoryCards

-- lead :: [Card] -> [Card] -> Card
-- lead hand previous 
--   | haveCard hand (Card Club Two) = Card Club Two 
--   | haveSuit previous Heart = select hand 
--   | otherwise = select $ filter (\x -> getSuit x /= Heart) hand
--     where
--       select :: [Card] -> Card
--       select [] = hand !! getRandomIndex hand
--       select list = list !! getRandomIndex list

-- getRandomIndex :: [Card] -> Int
-- getRandomIndex list = fst (randomR (0, length list - 1) (mkStdGen 123))


lead :: [Card] -> [Card] -> Card
lead hand previous 
  | haveCard hand (Card Club Two) = Card Club Two 
  | otherwise = getSmallestVoidCard cards
  where 
    cards = filterBreaking (not $ haveSuit previous Heart) hand
  
  -- | haveCard previous (Card Spade Queen) = undefined
  -- | haveCard cards (Card Spade Queen) = undefined

filterBreaking :: Bool -> [Card] -> [Card]
filterBreaking False hand = hand
filterBreaking True hand = if length newCards == 0 then hand else newCards 
  where 
    newCards = filter (\x -> getSuit x /= Heart) hand 

filterBleeding :: Bool -> [Card] -> [Card]
filterBleeding False hand = hand
filterBleeding True hand = if length newCards == 0 then hand else newCards
  where 
    newCards = filter (\x -> getSuit x /= Heart && x /= Card Spade Queen) hand

renege :: String -> Card -> [Card] -> [Card] -> [Card] -> Card
renege style leadCard hand current previous = case style of 
  -- bleed only happens for first time
  "bleed" | haveLeadingSuit -> getLargestCard hand leadSuit
  -- getLargestCard bleedCards leadSuit
  -- getLargestSafeCard (filterSuit hand leadSuit) (maximum $ filterSuit current leadSuit)
  -- minimum $ filterSuit bleedCards leadSuit
  -- minimum $ filterSuit hand leadSuit
          | otherwise -> getLargestVoidCard bleedCards

  -- if have leadSuit and have Spade Queen in current trick, then throw the safest card below Queen
  "nonBleed"  | haveLeadingSuit && haveCard current (Card Spade Queen) 
                -> getLargestSafeCard (filterSuit hand leadSuit) (maximum $ filterSuit current leadSuit)
                
              | haveLeadingSuit && not (haveCard current (Card Spade Queen)) 
                -> if haveCard previous (Card Spade Queen) 
                    then getLargestCard hand leadSuit 
                  else getLargestSafeCard (filterSuit hand leadSuit) (maximum $ filterSuit current leadSuit)
                  -- else getLargestSafeCard (filterSuit hand leadSuit) (getLargestPointCard current leadSuit)
              
              | not haveLeadingSuit && haveCard hand (Card Spade Queen) -> Card Spade Queen
              
              | not haveLeadingSuit && haveSuit previous Heart 
              -- | not haveLeadingSuit
                -> if haveSuit hand Heart 
                    then getLargestCard breakCards Heart
                  else getLargestVoidCard breakCards
                
              | not haveLeadingSuit && not (haveSuit previous Heart)
                -> if haveSuit hand Heart 
                    then getLargestCard hand Heart
                  else getLargestVoidCard hand
              
              | otherwise -> getLargestVoidCard breakCards

  _       ->    error "not a valid style"
  where
    leadSuit = getSuit $ leadCard
    haveLeadingSuit = haveSuit hand leadSuit
    breakCards = filterBreaking (not (haveSuit previous Heart || haveSuit current Heart)) hand 
    bleedCards = filterBleeding True hand

-- [C10,CQ,CJ,H3,H5,H10,C8,HJ,SJ]
-- [Card Club Ten, Card Club Queen, Card Club Jack, Card Heart Three, Card Heart Five, Card Heart Ten, Card Club Eight, Card Heart Jack, Card Spade Jack]
-- Card Club Ten, Card Club Queen, Card Heart Jack
-- getLargestSafeCard (filterSuit [Card Club Ten, Card Club Queen, Card Club Jack, Card Heart Three, Card Heart Five, Card Heart Ten, Card Club Eight, Card Heart Jack, Card Spade Jack] Club) (Card Club Seven)
-- [Card Spade Ace, Card Spade Four, Card Spade Ten, Card Heart Ace, Card Diamond Ace, Card Heart Three, Card Diamond Four]
getSmallestVoidCard :: [Card] -> Card
getSmallestVoidCard hand = getSmallestRank $ voidCards hand

getLargestVoidCard :: [Card] -> Card
getLargestVoidCard hand = getLargestRank $ voidCards hand

voidCards :: [Card] -> [Card]
voidCards hand = filteredVoids >>= (\(x, _) -> filterSuit hand x)
  where 
    voids = filter (\(_, a) -> if a > 0 then True else False) $ quantifySuits <$> [Club, Heart, Spade, Diamond]
    least = getLeastNumber voids
    filteredVoids = filter (\(_, a) -> if a == least then True else False) voids

    quantifySuits :: Suit -> (Suit, Int)
    quantifySuits suit = (suit, length $ filterSuit hand suit)
  
    getLeastNumber :: [(Suit, Int)] -> Int
    getLeastNumber pairs = foldl (\a (_, b) -> if b < a && b > 0 then b else a) (snd $ head pairs) pairs

getSmallestRank :: [Card] -> Card 
getSmallestRank cards = foldl (\a b -> if getRank b < getRank a then b else a) (head cards) cards 

getLargestRank :: [Card] -> Card 
getLargestRank cards = foldl (\a b -> if getRank b > getRank a then b else a) (head cards) cards 

-- takes in a filtered cards of the given card suit
getLargestSafeCard :: [Card] -> Card -> Card
getLargestSafeCard hand card = foldl (\a b -> if b > a && b < card then b else a) (minimum hand) hand

-- getSmallestSafeCard :: [Card] -> Card -> Card
-- getSmallestSafeCard hand card = foldl (\a b -> if b > a && b < card then b else a) (maximum hand) hand

-- returns the largest point card of the tricks thrown
-- getLargestPointCard :: [Card] -> Suit -> Card 
-- getLargestPointCard tricks leadSuit 
--   | haveCard tricks (Card Spade Queen) = Card Spade Queen
--   | haveSuit tricks Heart = maximum $ filterSuit tricks Heart
--   | otherwise = maximum $ filterSuit tricks leadSuit

-- [Card Spade Ace, Card Spade Four, Card Spade Ten, Card Heart Ten, Card Diamond Ace] (Card Spade Queen)
getLargestCard :: [Card] -> Suit -> Card
getLargestCard hand suit = maximum (filterSuit hand suit)



-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
