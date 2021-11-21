-- | Write a report describing your design and strategy here.
module ShunPlayer (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types

-- Nicholas: You need to read Types.hs and Cards.hs at minimum to start doing things.
-- Play.hs contains the sequence of play for every round, but it's not necessary to fully understand it.
-- You only need to know how the game works, and you just need to play a single card, everything else is automated.
-- The problem is choosing the right card to play. This is what you'll need to solve.

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] _ = (lead hand,"")
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick _ = (renege (fst $ last trick ) hand , "")

-- | The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- Will throw an error if no cards of that suit is available. This renege is incomplete.
renege :: Card ->[Card] -> Card 
renege leader hand  = select (find (\x -> suit x == suit leader)hand) where
    select :: Maybe Card -> Card
    --Select biggest card in hand if no cards match the leader suit
    select Nothing
     --Throw Queen of Spades first to prevent accumulating score
     | filter (\x -> suit x /=Heart) hand==[] && filter (\x -> suit x == Spade && rank x == Queen) hand/=[]= head $filter (\x -> suit x == Spade && rank x == Queen)hand 
     --Throw biggest heart in hand if there are no other cards to satisfy the bleeding rule
     | filter (\x -> suit x /=Heart) hand==[] = maximum$filter (\x -> suit x == Heart)hand
     --Throw biggest non-point cards first
     | otherwise= maximum$filter (\x -> suit x /= Heart)hand 
     
    select _
    --Only throw Queen of Spades if there is no other Spade cards to satisfy the reneging rule
     | suit leader==Spade && filter (\x -> suit x == suit leader && rank x /= Queen) hand ==[]= head$filter (\x -> suit x == suit leader)hand
    --Immediately throw Queen of Spades if the leader rank is greater than Queen 
     | suit leader==Spade && rank leader>Queen &&filter(\x -> suit x == suit leader && rank x==Queen)hand/=[]
     = head$filter(\x -> suit x == suit leader && rank x==Queen)hand
     
     --Throw biggest card in hand that matches the leader suit and is smaller than leader rank
     | filter (\x -> suit x == suit leader&& rank x <= rank leader)hand /=[]= maximum(filter (\x -> suit x == suit leader&& rank x <= rank leader)hand)
     | otherwise = minimum(filter(\x -> suit x == suit leader)hand)
    


-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two))hand) where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Maybe Card -> Card
    select Nothing 
    -- Select smallest heart if there are no other existing cards
     | filter (\x -> suit x /= Heart) hand ==[] = minimum(hand)
    --Select smallest card in deck to lead to minimize chances of accumulating score
     | otherwise = minimum(filter (\x -> suit x /= Heart)hand)
    --If there is 2 of clubs in hand throw two of clubs
    select card = fromJust card

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- | Given a card, select its rank.
rank :: Card -> Rank
rank (Card _ r) = r

-- | Find maximum in a list


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
