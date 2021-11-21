{-OVERALL GAME STRATEGY
In any Hearts Game there is 2 Playstyles with each playstyle having 2 types of players each having their own unique strategy, these are:

PLAYSTYLE 1: Conservative Play
Assuming there is no sufficient high cards to shoot the moon, this is the standard strategy.
    A.) Non SQ Holders: A non SQ holder's goal is to:
        1. Avoid leading (except for the first few rounds)
        2. Getting rid of high (risky) cards whenever the opportunity arises
        3. Conserving low (safe) cards to stay out of leading
    Ideally by mid-late game, they would have gotten rid of the larger cards and can safely stay in renege with low cards.

    B.) SQ Holders: An SQ holder's goal is 
        1. Void Diamonds & Clubs as soon as possible
        2. Stay in leading to easily focus on getting rid of Diamonds & Clubs
    Ideally by mid game have either no Diamonds/Clubs or both to get rid of SQ during a Diamond/Club trick.
    This playstyle is weak against Shoot the Moon AIs

PLAYSTYLE 2: Shoot the Moon Bait Play
Assuming there is sufficient high cards to attempt to shoot the moon, this is the highly risky but rewarding strategy
This playstyle is designed to counter Conservative AIs whose main priorities are to rid themselves of all their hearts & SQ
By "baiting" these AIs with high value cards, there is a good chance that we are able to collect all point cards.
    A.) Non SQ Holders: A non SQ holder's goal is to:
        1. Stay in leading and keep playing the highest cards to do so
        2. Collect SQ by playing Risky spade
        3. Collect all hearts by either playing all hearts or depleted Suit cards

    B.) SQ Holders: An SQ holder's goal is 
        1. Stay in leading and keep playing the highest cards to do so
        2. Play SQ in a Spade round and make sure it is the highest spade card
        3. Collect all hearts by either playing all hearts or depleted Suit cards

    This playstyle is weak against Dumb AI as they play cards randomly AND AIs who can identify a "Shoot the moon" attempt
    who can intercept by sacrificing/withold 1 low heart card in order to prevent a shoot the moon AI from collecting all the point cards
-}
module AlfonsPlayer (
    playCard,
    makeBid,
    lead,
    renege
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types

playCard :: PlayFunc
-- PlayFunc passes in 4 variables   
-- PlayerId -- this player's Id so they can identify themselves in the bids and tricks
-- [Card]   -- the player's cards
-- [(Card, PlayerId)]    -- cards in the current trick, so far
-- Maybe ([(Card, PlayerId)], String) -- previous player's state
{-NOTE: REMEMBER you only control 1 player, so every round you either lead or renege
So memory only works if you update it every time its your turn-}
-- [C2, C4, C5, C9]-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] previous = (lead hand (getMemoryList previous), (writeMemory (getMemoryString previous) (getPreviousTrick previous)))
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick previous = (renege trick hand (getMemoryList previous), (writeMemory (getMemoryString previous) (getPreviousTrick previous)))
 
{-
Notes:
  1.) Order of Suite Ascending = Spade, Club, Diamond, Heart
  2.) select (find (== (Card Club Two)) hand)
  3.) reverse . sort $ [Card Spade Two, Card Spade Four, Card Spade Ace, Card Spade Queen, Card Spade King, Card Spade Jack]
  4.) length ([Card]) returns amount of cards
  5.) fst is a built in function to access the first of a tuple (scnd, etc...)
  6.) $ is (...)
  7.) <$> is fmap, example of use: show <$> fst <$> list, [(Card Club Two, "1"), (Card Club Three, "2")] -> [C2] -> ["C2"]
-}

-- ====================================================================================== --
--                           PATTERN MATCHING FUNCTIONS                                   --
-- ====================================================================================== --
{- NAMING NOMENCLATURE:
    () optional <> compulsary
    [FORMAT]
    f(Risky|Safe)<suit|card><Renege|Lead>
        - Risky cards are those whose rank is either >Queen or >=Jack
        - Vice versa for safe
        - Sometimes Suit/Card names will be abbreviated (Ex: fRiskyDiamondClub -> fRiskyDC, fQueenOfSpades -> fSQ)
    Ex: fSQRenege filters for Queen of Spades with "Ren" suggesting that this function is made for renege
-}

----- {CARD SPECIFIC} ----- 
fTwoClubs :: [Card] -> [Card]
fTwoClubs = filter (\x -> rank x == Two) . fClubs

fSQ :: [Card] -> [Card]
fSQ = filter (\x -> rank x == Queen) . fSpades

----- {DIAMONDS & CLUBS} -----
fDiamonds :: [Card] -> [Card]
fDiamonds = filter (\x -> suit x == Diamond)

fRiskyDiamonds :: [Card] -> [Card]
fRiskyDiamonds hand = filter (\x -> rank x >= Jack) (fDiamonds hand)

fClubs :: [Card] -> [Card]
fClubs = filter (\x -> suit x == Club)

fRiskyClubs :: [Card] -> [Card]
fRiskyClubs hand = filter (\x -> rank x >= Jack) (fClubs hand)

fRiskyDC :: [Card] -> [Card]
fRiskyDC hand = fRiskyDiamonds hand ++ fRiskyClubs hand

----- { HEARTS } -----
fHearts :: [Card] -> [Card]
fHearts = filter (\x -> suit x == Heart)

----- { SPADE } -----
fSpades :: [Card] -> [Card]
fSpades = filter (\x -> suit x == Spade)

fRiskySpades :: [Card] -> [Card]
fRiskySpades = filter (\x -> rank x > Queen) . fSpades

fSafeSpades :: [Card] -> [Card]
fSafeSpades = filter (\x -> rank x < Queen) . fSpades

-- Returns a list of cards of the same suit as leadSuit AND lower rank than highest card in trick
--     suit_leader-> trick  -> hand
fSuit :: Suit -> [Card] -> [Card]
fSuit leadSuit = filter (\x -> suit x == leadSuit)

fHighestCard :: [Card] -> Card
fHighestCard = head . sortDesc

fSortByRank :: [Card] -> [Card]
fSortByRank hand = filter(\x -> rank x == Two) hand ++ filter(\x -> rank x == Three) hand ++ filter(\x -> rank x == Four) hand ++ filter(\x -> rank x == Five) hand ++ filter(\x -> rank x == Six) hand ++ filter(\x -> rank x == Seven) hand ++
               filter(\x -> rank x == Eight) hand ++ filter(\x -> rank x == Nine) hand ++ filter(\x -> rank x == Ten) hand ++ filter(\x -> rank x == Jack) hand ++ filter(\x -> rank x == Queen) hand ++ filter(\x -> rank x == King) hand ++ filter(\x -> rank x == Ace) hand

fSortByRankDesc :: [Card] -> [Card]
fSortByRankDesc hand = filter(\x -> rank x == Ace) hand ++ filter(\x -> rank x == King) hand ++ filter(\x -> rank x == Queen) hand ++ filter(\x -> rank x == Jack) hand ++ filter(\x -> rank x == Ten) hand ++ filter(\x -> rank x == Nine) hand ++ 
                filter(\x -> rank x == Eight) hand ++ filter(\x -> rank x == Seven) hand ++ filter(\x -> rank x == Six) hand ++ filter(\x -> rank x == Five) hand ++ filter(\x -> rank x == Four) hand ++ filter(\x -> rank x == Three) hand ++ filter(\x -> rank x == Two) hand

-- ================================================================================================================= --
--                                                LEAD FUNCTION                                                      --
--         The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.       --                                                              --
-- ================================================================================================================= --
{-Lead Strategy:
    Arrange the cards in a hand based on a pre-determined "priority list".
    The card to be played at any round will be the first card in this list.
    ----------[LEAD PRIORITY LIST]----------
    [A] Non SQ Holder
        1.) Two of Clubs
            Reason: Game rules state that C2 must be played first
        2.) (Early Game first 3 rounds) Risky Club/Diamond
            Reason: First 3 rounds are relatively safe to throw away risky Diamond/Club cards
        3.) Lowest Club, Diamond, Spade (also Heart if hearts broken) [D2, C2, S3, H3, ...]
            Reason: After early game, try to get out of lead by playing lowest non point card possible
        4.) Risky Spades (Spades > Queen)
            Reason: Last resort lead, risky spades should be played in a non-match renege
        5.) Queen of Spades
            Reason: SQ should NOT be played on Lead as it would lead to the leader getting it.

    [B] SQ Holder
        1.) Two of Clubs
        2.) (Early Game first 3 rounds) Risky Club/Diamond
        3.) Highest/Diamond Club if still having at least one card of both Suits
            Reason: Focus on voiding either Diamond/Club to get rid of SQ in renege. 
            It should be safe to stay in leading until mid game as we hold the SQ.
        4.) Lowest Club, Diamond (also Heart if hearts broken) [D2, C2, S3, H3, ...]
            Reason: After depleting a Suit, get out of leading ASAP and hope another player will lead with the depleted Suit to get rid of SQ
        5.) Lowest Safe Spades (Spades < Queen)
            Reason: Stay out of lead
        6.) Queen of Spades
-}

lead :: [Card] -> [Card] -> Card
-- Holding SQ
lead hand memory 
    | queenOfSpadesHolder hand =
        select $ (fTwoClubs hand ++ 
        fEarlyRiskyLead hand memory ++
        fSafeLead hand memory ++
        fRiskySpades hand ++
        fSQ hand)

--Not Holding SQ
    | otherwise =
        select $ (fTwoClubs hand ++ 
        fEarlyRiskyLead hand memory ++
        fSafeLead hand memory ++
        fRiskySpades hand ++
        fSQ hand)
    where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: [Card] -> Card
    select [] = head hand
    select x = head x

    -- Test case
    -- fEarlyRiskyLead ([Card Club Eight, Card Club Jack, Card Diamond Ace, Card Diamond King]) 
    --([Card Club Two, Card Club Three, Card Club Four, Card Club Five, Card Diamond Two, Card Diamond Three, Card Diamond Four, Card Diamond Five])
    fEarlyRiskyLead :: [Card] -> [Card] -> [Card]
    fEarlyRiskyLead handList memoryList
        | (length memoryList <= 4) = fRiskyDC handList
        | otherwise = []

    fSafeLead :: [Card] -> [Card] -> [Card]
    fSafeLead handList memoryList
        | (heartsBroken memoryList == True) = fSortByRank $ fDiamonds handList ++ fClubs handList ++ fHearts handList ++ fSafeSpades handList
        | otherwise = fSortByRank $ fDiamonds handList ++ fClubs handList ++ fSafeSpades handList
                                     
-- ================================================================================================================= --
--                                                RENEGE FUNCTION                                                    --
-- Renege function takes in current trick and selects the first occuring card of that suit from a priority list      --                                                              --
-- ================================================================================================================= --
{-Renege Strategy:
    Arrange the cards in a hand based on a pre-determined "priority list".
    The card to be played at any round will be the first card in this list.
    ----------[RENEGE PRIORITY LIST]----------
    [A] Non SQ holder
        1.) (First Round) Highest Club
            Reason: The first round is safe from point cards
        2.) (Second & Third Round) Risky Club/Diamond
            Reason: The second & third rounds are relatively safe from point cards
        3.) Highest "Safe" Card with Leader's Suit (Safe means lower than the highest same-suit card of the current trick)
            Reason: Staying out of lead for next round while conserving lower cards for later rounds
        4.) Lowest "Risky" Card with Leader's Suit
            Reason: Hope that someone else plays a higher card to avoid leading next round
        5.) Queen of Spade
            Reason: Get rid of SQ as soon as possible
        6.) Risky Spades (Spades > Queen)
            Reason: Risky Spades attract point cards during a Spade trick
        7.) Risky Diamonds & Clubs (Diamonds & Clubs > Jack) <--- Can be lowered
            Reason: Risky Diamonds attract point cards during a Diamond trick
        8.) Highest Hearts
            Reason: Break & Throw high hearts in non heart suit
        9.) Safe Diamonds & Clubs
            Reason: Safe non-point cards should be used to get out of leading not discarded in renege
        10.) Safe Spades
            Reason: Safe Spades are useful to bleed spades and are a very useful card to lead

    [B] SQ holder
        1.) (First Round) Highest Club
            Reason: The first round is safe from point cards
        2.) (Second & Third Round) Risky Club/Diamond
            Reason: The second & third rounds are relatively safe from point cards
        3.) Highest "Safe" Card with Leader's Suit (Safe means lower than the highest same-suit card of the current trick)
            Reason: Staying out of lead for next round while conserving lower cards for later rounds
        4.) Lowest "Risky" Card with Leader's Suit
            Reason: Hope that someone else plays a higher card to avoid leading next round
        5.) Queen of Spade
            Reason: Get rid of SQ as soon as possible
        6.) Risky Spades (Spades > Queen)
            Reason: Risky Spades attract point cards during a Spade trick
        7.) Risky Diamonds & Clubs (Diamonds & Clubs > Jack) <--- Can be lowered
            Reason: Risky Diamonds attract point cards during a Diamond trick
        8.) Highest Hearts
            Reason: Break & Throw high hearts in non heart suit
        9.) Safe Diamonds & Clubs
            Reason: Safe non-point cards should be used to get out of leading not discarded in renege
        10.) Safe Spades
            Reason: Safe Spades are useful to bleed spades and are a very useful card to lead


-}
--          current trick    ->  hand  -> memory -> chosen card 
renege :: [(Card, PlayerId)] -> [Card] -> [Card] -> Card
renege trick hand memory
    | queenOfSpadesHolder hand =
        select $ (firstRound hand (leader trick) ++
        secondThirdRound hand memory (suit $ leader trick) ++ 
        fSuitSafe (suit $ leader trick) (simpleTrick trick) hand ++
        fSuitRisky (suit $ leader trick) (simpleTrick trick) hand ++
        fSQRenege (leader trick) hand ++
        fRiskySpades hand ++
        fRiskyDC hand ++
        (sortDesc $ fHeartsRenege (leader trick) hand) ++
        fSafeDCRenege hand ++
        (sortDesc $ fSpades hand))

    | otherwise =
        select $ (firstRound hand (leader trick) ++
        secondThirdRound hand memory (suit $ leader trick) ++ 
        fSuitSafe (suit $ leader trick) (simpleTrick trick) hand ++
        fSuitRisky (suit $ leader trick) (simpleTrick trick) hand ++
        fSQRenege (leader trick) hand ++
        fRiskySpades hand ++
        fRiskyDC hand ++
        (sortDesc $ fHeartsRenege (leader trick) hand) ++
        fSafeDCRenege hand ++
        (sortDesc $ fSpades hand))
    where
    -- | Select the first card from hand if no particular card was provided.
    select :: [Card] -> Card
    select [] = head hand
    select x = head x

    firstRound :: [Card] -> Card -> [Card]
    firstRound handList leadCard
        | leadCard == (Card Club Two) = sortDesc $ fClubs handList
        | otherwise = []

    secondThirdRound :: [Card] -> [Card] -> Suit -> [Card]
    secondThirdRound handList memoryList leadSuit
        | (length memoryList > 8) = []
        | (leadSuit == Club) = sortDesc $ fClubs handList
        | (leadSuit == Diamond) = sortDesc $ fDiamonds handList
        | otherwise = []

    fSuitSafe :: Suit -> [Card] -> [Card] -> [Card]
    fSuitSafe leadSuit thisTrick handList = sortDesc $ filter (\x -> suit x == leadSuit && rank x < (rank $ fHighestCard $ fSuit leadSuit thisTrick)) handList
        
    fSuitRisky :: Suit -> [Card] -> [Card] -> [Card]
    fSuitRisky leadSuit thisTrick handList = sort $ filter (\x -> suit x == leadSuit && rank x > (rank $ fHighestCard $ fSuit leadSuit thisTrick)) handList

    fSQRenege :: Card -> [Card] -> [Card]
    fSQRenege (Card Club Two) _ = []
    fSQRenege _ handList = filter (\x -> rank x == Queen && suit x == Spade) handList
    
    fHeartsRenege :: Card -> [Card] -> [Card]
    fHeartsRenege (Card Club Two) _ = []
    fHeartsRenege _ handList = filter (\x -> suit x == Heart) handList

    fSafeDCRenege :: [Card] -> [Card]
    fSafeDCRenege handList = fSortByRankDesc $ fDiamonds handList ++ fClubs handList

-- ====================================================================================== --
--                                  HELPER FUNCTIONS                                      --
-- ====================================================================================== --
        
-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

rank :: Card -> Rank
rank (Card _ r) = r

leader :: [(Card, PlayerId)] -> Card
leader trick = fst $ last trick

sortDesc :: [Card] -> [Card]
sortDesc hand = reverse . sort $ hand

simpleTrick :: [(Card, PlayerId)] -> [Card]
simpleTrick = map (\(a,_) -> a)

heartsBroken :: [Card] -> Bool
heartsBroken memoryList = (fHearts memoryList) /= []

queenOfSpadesHolder :: [Card] -> Bool
queenOfSpadesHolder handList = (fSQ handList) /= []

-- (writeMemory (getMemoryString previous) (getPreviousTrick previous))

-- toString extracts the tuple of cards from the previous trick and converts it into a string representation
-- Maybe ([(Card, PlayerId)], String): 
--(Just ([(Card Club Two, "1"), (Card Heart Three, "2"), (Card Spade Seven, "3"), (Card Diamond Nine, "4")], ""))
-- String: "S2 S3 S7 S9" which can be concatenated to memory string
-- Use fmap twice to convert  [(Card Club Two, "1"), (Card Heart Three, "2"), (Card Spade Seven, "3"), (Card Diamond Nine, "4")]
--      into ["S2", "S3", "S7", "S9"]
-- Use intercalate to format this list as "S2 S3 S7 S9"
toString :: Maybe ([(Card, PlayerId)], String) -> String
toString Nothing = []
toString (Just (list, _)) = intercalate " " (show <$> fst <$> list)

-- toList converts a string representation of cards into a list of cards
-- String: "S2 S3 S7 S9" -> [Card]: [S2, S3, S7, S9]
-- words "S2 S3 S7 S9" -> ["S2", "S3", "S7", "S9"]
-- Map every string in the above list with (read a :: Card) 
-- which converts a string "C2" into respective Card type C2

toList :: String -> [Card]
toList str =  map (\a -> (read a :: Card)) (words str)

cardListToString :: [Card] -> String
cardListToString list = intercalate " " (map (\a -> (show a)) list)
-- This is just the sequence of toString -> toList
-- If toString & toList are not used seperately down the line, try and merge them into this function
getPreviousTrick :: Maybe ([(Card, PlayerId)], String) -> [Card]
getPreviousTrick = toList . toString

getMemoryList :: Maybe ([(Card, PlayerId)], String) -> [Card]
getMemoryList = toList. getMemoryString

-- This is just cardListToString but with a few adjustmennts such as addition of space
-- If cardListToString is only used here then feel free to merge
writeMemory :: String -> [Card] -> String
writeMemory "" list = cardListToString list
writeMemory memory list = memory ++ " " ++ (cardListToString list)

-- Extracts just the String in a Maybe ([(Card, PlayerId)], String)
-- Used to get the string memory to concatenate cards in it for the next round
getMemoryString :: Maybe ([(Card, PlayerId)], String) -> String
getMemoryString Nothing = ""
getMemoryString (Just (_, str)) = str

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
