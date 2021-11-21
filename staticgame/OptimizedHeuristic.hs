-- | Write a report describing your design and strategy here.
module OptimizedHeuristic (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types
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
filterSuit hand s = filter ((s==).getSuit) hand

safeFilter :: Bool -> (Card -> Bool) -> [Card] -> [Card]
safeFilter False _ cards = cards
safeFilter True f cards = if length newCards == 0 then cards else newCards
  where newCards = filter f cards

getNewMemory :: Maybe ([(Card, PlayerId)], String) -> String
getNewMemory Nothing = ""
getNewMemory (Just (tricks, memory)) = memory ++ " " ++ intercalate " " (map (show.fst) tricks) 

-- convertFrom :: String -> Maybe ([(Card, PlayerId)], String) -> [Card]
-- convertFrom _ Nothing = []
-- convertFrom "tricks" (Just (tricks, _)) = map fst tricks
-- convertFrom "memory" (Just (_, memory)) = parseString (words memory)

convert :: String -> Maybe ([(Card, PlayerId)], String) -> [Card]
convert _ Nothing = []
convert "tricksFrom" (Just (tricks, _)) = map fst tricks
convert "memoryFrom" (Just (_, memory)) = parseString (words memory)
convert _ (Just _) = []

parseString :: [String] -> [Card]
parseString [] = []
parseString string = map (\x -> read x :: Card) string



-- playCard :: PlayFunc
-- playCard _ hand [] previousState = (lead hand previousCards, memory)
--   where
--     previousCards = convert "tricksFrom" previousState ++ convert "memoryFrom" previousState
--     memory = getNewMemory previousState
-- -- (renege hand currentCards previous, memory)
-- playCard _ hand tricks previous 
--   | memoryCards == [] = (renege "bleed" leadCard hand currentCards previousCards, memory)
--   | otherwise = (renege "nonBleed" leadCard hand currentCards previousCards, memory)
--   where
--     memoryCards = convert "tricks" previous
--     leadCard = fst $ last tricks
--     memory = getNewMemory previous
--     currentCards = map fst tricks
--     previousCards = convert "memory" previous ++ memoryCards
--     -- previousCards = convert "memoryFrom" previous ++ convert "tricksFrom" previous


playCard :: PlayFunc
playCard _ hand [] previousState = (lead hand previousCards, memory)
  where
    previousCards = convert "tricksFrom" previousState ++ convert "memoryFrom" previousState
    memory = getNewMemory previousState

playCard _ hand tricks previous = (renege hand currentCards previous, memory)
  where
    memory = getNewMemory previous
    currentCards = map fst tricks

renege :: [Card] -> [Card] -> Maybe ([(Card, PlayerId)], String) -> Card
renege hand currentCards Nothing = obtainCardFromBleeds bleedHand
  where 
    leadSuit = getSuit $ last currentCards
    -- bleedHand will only either be: [Card] without point cards OR [Card] with point cards only
    bleedHand = safeFilter True (\x -> getSuit x /= Heart && x /= Card Spade Queen) hand

    obtainCardFromBleeds :: [Card] -> Card
    -- TO BE MODIFIED: getLargestCard to avoid passing leadSuit
    -- if the [Card] without point cards have leading suit, then get largest, otherwise it means that [Card] have point cards only, so get the void card (of the largest card)
    obtainCardFromBleeds cards = if haveSuit cards leadSuit then getLargestCard cards leadSuit else getLargestVoidCard cards

renege hand currentCards previousState 
  -- if there is a Queen now or Queen has not been thrown, play safe prevent throwing highest card 
  | haveLeadingSuit && (currentHaveQueen || not previousHaveQueen) = getLargestSafeCard filteredHand maxCurrentCard 
  -- if Queen has been thrown, can throw high cards now
  | haveLeadingSuit && previousHaveQueen = getLargestCard filteredHand leadSuit
  -- if I have Queen
  | handHaveQueen = (Card Spade Queen)
  -- if I have hearts, throw the highest, to reduce risk
  | handHaveHearts = getLargestCard filteredHand Heart
  -- no hearts, switch strategy to create void - emptying suits with least number of cards, whether hearts have been broken or not, goal will still be to create void based on the cards (breakHand) that I have
  | otherwise = getLargestVoidCard breakHand

  where 
    leadSuit = getSuit $ last currentCards
    haveLeadingSuit = haveSuit hand leadSuit
    previousCards = convert "memoryFrom" previousState ++ convert "tricksFrom" previousState
    maxCurrentCard = maximum $ filterSuit currentCards leadSuit
    -- filteredHand will only either be: [Card] without leading suit OR [Card] with only leading suit
    filteredHand = safeFilter haveLeadingSuit ((leadSuit==).getSuit) hand
    handHaveQueen = haveCard hand (Card Spade Queen)
    handHaveHearts = haveSuit hand Heart

    previousHaveQueen = haveCard previousCards (Card Spade Queen)
    currentHaveQueen = haveCard currentCards (Card Spade Queen)
    
    previousHaveHearts = haveSuit previousCards Heart
    currentHaveHearts = haveSuit currentCards Heart
    heartsBroken = previousHaveHearts || currentHaveHearts

    -- if heartsBroken, then don't filter, let Hearts to be in hand, otherwise, hearts not broken, remove Hearts from hand
    breakHand = safeFilter (not (heartsBroken)) ((Heart/=).getSuit) hand 


lead :: [Card] -> [Card] -> Card
lead hand previous 
  | haveCard hand (Card Club Two) = Card Club Two 
  | otherwise = getSmallestVoidCard cards
  where 
    cards = safeFilter (not $ haveSuit previous Heart) ((Heart/=).getSuit) hand

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

compareRank :: (Rank -> Rank -> Bool) -> Card -> Card -> Card
compareRank f a b = if f (getRank b) (getRank a) then b else a 

getSmallestRank :: [Card] -> Card 
getSmallestRank cards = foldl (compareRank (<)) (head cards) cards

getLargestRank :: [Card] -> Card 
getLargestRank cards = foldl (compareRank (>)) (head cards) cards

-- takes in a filtered cards of the given card suit
getLargestSafeCard :: [Card] -> Card -> Card
getLargestSafeCard hand card = foldl (\a b -> if b > a && b < card then b else a) (minimum hand) hand

-- [Card Spade Ace, Card Spade Four, Card Spade Ten, Card Heart Ten, Card Diamond Ace] (Card Spade Queen)
getLargestCard :: [Card] -> Suit -> Card
getLargestCard hand suit = maximum (filterSuit hand suit)

-- renege :: Card -> [Card] -> [Card] -> [Card] -> Card
-- renege leadCard hand current previous = 

-- renege :: String -> Card -> [Card] -> [Card] -> [Card] -> Card
-- renege style leadCard hand current previous = case style of 
  
--   "bleed" | haveLeadingSuit -> getLargestCard hand leadSuit
--           | otherwise -> getLargestVoidCard bleedCards

--   "nonBleed"  | haveLeadingSuit && haveCard current (Card Spade Queen) 
--                 -> getLargestSafeCard (filterSuit hand leadSuit) (maximum $ filterSuit current leadSuit)
                
--               | haveLeadingSuit && not (haveCard current (Card Spade Queen)) 
--                 -> if haveCard previous (Card Spade Queen) 
--                     then getLargestCard hand leadSuit 
--                   else getLargestSafeCard (filterSuit hand leadSuit) (maximum $ filterSuit current leadSuit)
              
--               | not haveLeadingSuit && haveCard hand (Card Spade Queen) -> Card Spade Queen
              
--               | not haveLeadingSuit && haveSuit previous Heart 
--                 -> if haveSuit hand Heart 
--                     then getLargestCard breakCards Heart
--                   else getLargestVoidCard breakCards
                
--               | not haveLeadingSuit && not (haveSuit previous Heart)
--                 -> if haveSuit hand Heart 
--                     then getLargestCard hand Heart
--                   else getLargestVoidCard hand
              
--               | otherwise -> getLargestVoidCard breakCards

--   _       ->    error "not a valid style"
--   where
--     leadSuit = getSuit $ leadCard
--     haveLeadingSuit = haveSuit hand leadSuit
--     breakCards = safeFilter (not (haveSuit previous Heart || haveSuit current Heart)) (\x -> getSuit x /= Heart) hand 
--     bleedCards = safeFilter True (\x -> getSuit x /= Heart && x /= Card Spade Queen) hand
    -- balanceCards = remainingCards (current ++ previous ++ hand) 
    -- size = getTricksBalance current
    -- k = probabilitiesSearch hand current balanceCards size

-- [Club, Heart, Spade, Diamond] [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
-- played: [Card Club Two, Card Club Three, Card Club Four, Card Heart Two, Card Heart Three]
-- remainingCards :: [Card] -> [Card]
-- remainingCards played = (allCards \\ played)
--   where 
--     allCards = [Card Club, Card Heart, Card Spade, Card Diamond] <*> [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

-- -- balance: [Card Spade Ace, Card Spade Four, Card Spade Ten]
-- -- hand: [Card Spade Eight, Card Heart King, Card Heart Ace]
-- -- current: [Card Spade Two]
-- -- simulateCards [Card Spade Ace, Card Spade Four, Card Spade Ten] 2 = [[SA,S4],[SA,S10],[S4,S10]]
-- -- combine each possible initial states: 
-- -- [Card Spade Two] -> [Card Spade Eight, Card Heart King, Card Heart Ace] to become:
-- -- [[Card Spade Two, Card Spade Eight], [Card Spade Two, Card Heart King], [Card Spade Two, Card Heart Ace]]
-- -- probabilitiesSearch [Card Spade Ace, Card Heart King, Card Heart Ace] [Card Spade Two] [Card Spade King, Card Spade Four, Card Spade Ten, Card Heart Five, Card Diamond King, Card Club Eight] 3

-- -- TEST CASE:
-- -- probabilitiesSearch [Card Spade Ace, Card Spade Five, Card Spade Ten, Card Heart King, Card Heart Ace] [Card Spade Eight] [Card Spade King, Card Spade Four, Card Spade Six, Card Heart Five, Card Diamond King, Card Club Eight] 3
-- -- simulated cards are (including lead card, exclude my chosen card): 
-- -- [[S8,SK,S4],[S8,SK,S6],[S8,S4,S6],[S8,SK,H5],[S8,S4,H5],[S8,S6,H5],[S8,SK,DK],[S8,S4,DK],[S8,S6,DK],[S8,H5,DK],[S8,SK,C8],[S8,S4,C8],[S8,S6,C8],[S8,H5,C8],[S8,DK,C8]]
-- probabilitiesSearch :: [Card] -> [Card] -> [Card] -> Int -> [(Card, Int)]
-- probabilitiesSearch hand current balanceCards size = result
--   -- map fst filteredMax
--   -- map (leadCard:) cardsSimulated 
--   where 
--     cardsSimulated = simulateCards balanceCards (size-1)
--     leadCard = last current
--     result = map (getWinPoints leadCard cardsSimulated) hand
--     maxChance = getMaxChance result
--     filteredMax = filter (\(_, a) -> if a == maxChance then True else False) result

--     getMaxChance :: [(Card, Int)] -> Int
--     getMaxChance pairs = foldl (\a (_, b) -> if b > a && b >= 0 then b else a) (snd $ head pairs) pairs


-- getWinPoints :: Card -> [[Card]] -> Card -> (Card, Int)
-- getWinPoints leadCard simulatedCards myCard = (myCard, sum $ map winLose simulatedCards)
--   where
--     -- True [1] means trick is not taken (won) while False [0] means trick is taken (lose)
--     -- winLose (Card Heart Queen) (Card Heart King) [Card Spade Queen, Card Heart Two] 
--     winLose :: [Card]  -> Int
--     winLose simulatedPair = if (leadSuit /= mySuit || largestSuitCard /= myCard) then 1 else 0
--       where
--         leadSuit = getSuit leadCard
--         mySuit = getSuit myCard
--         largestSuitCard = getLargestCard (myCard:leadCard:simulatedPair) leadSuit
--       -- | leadSuit /= mySuit = 1
--       -- | largestSuitCard /= myCard = 1
--       -- | otherwise = 0

-- getTricksBalance :: [Card] -> Int
-- getTricksBalance current = 4 - length current

-- -- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
-- -- [Card Club Two, Card Club Three, Card Spade Eight, Card Heart Five, Card Diamond King]
-- -- simulateCards [Card Spade Ace, Card Spade Three, Card Spade Five, Card Heart Three, Card Heart Ten] 2
-- -- [[SA,S3],[SA,S5],[S3,S5],[SA,H3],[S3,H3],[S5,H3],[SA,H10],[S3,H10],[S5,H10],[H3,H10]]
-- simulateCards :: [Card] -> Int -> [[Card]]
-- simulateCards cards size = filter ((size==).length) (subsequences cards)
-- -- simulateCards cards size = filter (\x -> length x == size) (subsequences cards)






-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
