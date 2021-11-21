-- | SOME IMPORTANT STUFFS:
-- | If you encounter TimeError, please LOWER down the number of loops in the 3rd argument of monteCarloSearch function inside playCard, 
-- | which guarding how much loops MCTS can go maximum.
-- | Please DO NOT use player ID of "Opponent"!! Reasons stated in Implementation part of this report below.

-- | In this assignment, I'm using Monte Carlo tree search as the AI strategy for this game.

-- | Introduction:
-- | In computer science, Monte Carlo tree search (MCTS) is a heuristic search algorithm for some kinds of decision
-- | processes, most notably those employed in game play. MCTS was introduced in 2006 for computer Go. It has been
-- | used in other board games like chess and shogi, games with incomplete information such as bridge and poker, as
-- | well as in real-time video games (such as Total War: Rome II's implementation in the high level campaign AI).

-- | A bit of history:
-- | In 2006, inspired by these predecessors, Rémi Coulom described the application of the Monte Carlo method to game-tree
-- | search and coined the name Monte Carlo tree search, L. Kocsis and Cs. Szepesvári developed the UCT (Upper Confidence
-- | bounds applied to Trees) algorithm, and S. Gelly et al. implemented UCT in their program MoGo. In 2008, MoGo
-- | achieved dan (master) level in 9×9 Go, and the Fuego program began to win against strong amateur players in 9×9 Go.
-- | In January 2012, the Zen program won 3:1 in a Go match on a 19×19 board with an amateur 2 dan player. Google
-- | Deepmind developed the program AlphaGo, which in October 2015 became the first Computer Go program to beat
-- | a professional human Go player without handicaps on a full-sized 19x19 board. In March 2016, AlphaGo was
-- | awarded an honorary 9-dan (master) level in 19×19 Go for defeating Lee Sedol in a five-game match with a final
-- | score of four games to one. AlphaGo represents a significant improvement over previous Go programs as well as
-- | a milestone in machine learning as it uses Monte Carlo tree search with artificial neural networks (a deep
-- | learning method) for policy (move selection) and value, giving it efficiency far surpassing previous programs.
-- | MCTS algorithm has also been used in programs that play other board games (for example Hex, Havannah,
-- | Game of the Amazons, and Arimaa), real-time video games (for instance Ms. Pac-Man and Fable Legends),
-- | and nondeterministic games (such as skat, poker, Magic: The Gathering, or Settlers of Catan).

-- | Principle of Operation:
-- | The focus of MCTS is on the analysis of the most promising moves, expanding the search tree based
-- | based on many playouts also called roll-outs. In each playout, the game is played out to the very
-- | end by selecting moves at random. The final game result of each playout is then used to weight
-- | the nodes in the game tree so that better nodes are more likely to be chosen in future playouts.
-- | on random sampling of the search space. The application of Monte Carlo tree search in games is
-- | The most basic way to use playouts is to apply the same number of playouts after each legal move of the current
-- | player, then choose the move which led to the most victories. The efficiency of this method—called Pure Monte Carlo
-- | Game Search—often increases with time as more playouts are assigned to the moves that have frequently resulted in the
-- | current player's victory according to previous playouts. Each round of Monte Carlo tree search consists of four steps:
-- | Selection;
-- | Expansion;
-- | Simulation and
-- | Backpropagation

-- | Implementation:
-- | In my implementation, I divide the whole program into 3 parts:
-- | memory handling;
-- | card selection handling and
-- | Monte Carlo tree search

-- | In the memory handling part, I handle the memory passed here according to 3 conditions.
-- | First, when I'm dealing with first round of trick (when the previous memory passed is Nothing), I'll
-- | just return an empty string back to play function.In second round, I can now know how much player I'm
-- | playing aginst with, which is the number of cards in the previous (first) trick of the game, I convert
-- | all the cards from the previous trick into single string and concat with a newly created "score board"
-- | with player IDs and their respective score,I can get all the player IDs of my opponents by extracting
-- | from the previous trick. Besides of the real players in the game, I also add another one player with with
-- | an ID of "Opponent", this is the virtual opponent I'm fighting against when performing MCTS simuation.
-- | Example output for second round:
-- | "CK,C2;ABC:0,DEF:0,Gge:0,YThv:0,Opponent:0"
-- | * there is a bug where I directly initialize all the players' score to 0, which I
-- | didn't catch the condition where one of the player will break the Heart in first
-- | round, but this kind of situation has the probability of roughly 1 in 45 billion only.
-- | In second round onwards, I'm dealing the memory string similarly as above by converting all the cards from previous
-- | round into string and concat with "score board" but in second round onwards, I'll update all the players' scores
-- | in the board (second half of my memory string) before I concatenate with the cards played. I update the players'
-- | score by using some string functions like in Python or other languages, e.g. split, string search, etc to parse
-- | the score board into list of tuples then replace the tuple of winner of last trick into new calculated score.
-- | Example output for second round onwards:
-- | "CK,CA,CJ,C2;ABC:0,DEF:13,Gge:0,YThv:2,Opponent:0"

-- | In card slection part, I use the hybrid of random and heuristic strategy.
-- | Before hand, I define my own seed instead of getting seeds from randomIO, because randomIO will return number in
-- | the context of IO, which I cannot extract it into pure number and it will be hard to implement a playCard function
-- | with IO context. I define my own seed using a formula which is similar to common hash function that I've learnt in
-- | FIT2004, in detail, I concat all the cards in the current trick into a single string and extract all of the numeric
-- | character in this string, for example if my current trick is [(Card Club Seven,"ggy"),(Card Club Ace,"jos"),(Card
-- | Spade Nine,"678")], after concatenation it becomes "C7CAS9", then I extract out all the numeric characters, "79",
-- | finally I read this "79" string into Int and do some hash function to get back a relatively random number. This
-- | random number will be used as a part of another seed or can be used in other ways. For another simRandom function,
-- | it also generate a seed value but in addition of using the random number above, I add it with the number of winning
-- | and simulations in MCTSNode to maximize the randomness of the returned number when doing simuation in MCTS.
-- | In renege function, I'll choose the card to discard with some heuristic strategy. First, I'll try to select
-- | the maximum card which has the same suit with leading card. If I don't have the card which is smaller than
-- | leader, I'll choose the maximum card of tha suit instead, becuase I'm losing the game anyway, so it is always
-- | good to lose the large card as soon as possible. If I don't have any card which has the same suit with leader,
-- | I'll filter out all non-point cards if it is the first round of the game. Otherwise, I'll just discard the
-- | maximum point card in my hand, again, to get rid of big cards especially point cards or Queen of Spade ASAP.
-- | If I'm leading in one game, I'll use leading function instead. In leading function, I'll try to select
-- | Two of Clubs if possible in first round. In second round onwards, I just simply flter out all the
-- | eligible cards in my hands. I'll use a function to check if the Hearts has been selected before (Heart
-- | is broken), if it is, all the cards in my hand will be eligible, if it doesn't, I'll filter out Hearts.
-- | Both of the above functions (renege and lead) are returning a list of eligible cards. So
-- | the card picking function is going to randomly choose one card using the seed generated
-- | above to randomly select one card in the eligible card list and return this single card.

-- | From here onwards, we are going to discuss about the most important part in my implmentation of this AI, which
-- | is Monte Carlo tree search. Same as the introduction above, Monte Carlo tree search is an algorithm of hybrid
-- | of probabilistic analysis and MinMax. But the original and most common version of MCTS is only applicable for
-- | perfect information 1v1 game like chess, go, tic tac toe, etc. So, in the MinMax tree of the MCTS, it is easy to
-- | implement the tree as one level for self and another level for opponent, same goes to the levels deeper below
-- | using this alternating style. But, I've modified the tree to make it somehow suitable for imperfect non-2-player
-- | game like Hearts. What I've done is, firstly, I integrate all 3 opponents into one "big" virtual player called
-- | "Opoonent", instead of seperating the level of tree by taking turn of self and ONE opponent, I'm separating the
-- | tree level into self and not only opponent, it can be self-self-opponent-oponent down to the tree as the players
-- | are not taking strictly turn in game of Hearts, the sequence of player depending on who is the winner of the
-- | last trick, that's why it can be my turn, then my turn also in the next round in case I won the previous trick.
-- | Another additional implementation which is different from the "proper" MCTS is I
-- | add another one phase call determinization, like the paragraphs from this website:
-- | http://www.aifactory.co.uk/newsletter/2013_01_reduce_burden.htm, which states that: 

-- | "Many common approaches to AI for games with hidden information use determinization. The idea is to guess
-- | the hidden information (for example randomly deal out the unseen cards to the other players), and make a
-- | decision based on a representative sample of such guesses (typically tens or hundreds, but sometimes more).
-- | Of course the individual guesses are likely to be wrong, but by taking a large number of these guesses, the
-- | hope is that we can collect enough information to assess which move is the best on average. One popular
-- | technique based on determinization is Perfect Information Monte Carlo (PIMC), which simply treats each
-- | determinization as if it were a game of perfect information (e.g. a card game with all cards face up),
-- | applies standard AI techniques to each of these games, and uses a majority vote to choose which move to play.

-- | PIMC has proven successful in several games, particularly Contract Bridge (Ginsberg 2001). However,
-- | the approach has two serious flaws. First, treating the game as one of perfect information means the
-- | AI will not see the value of gathering or hiding information, and will wrongly assume that it can
-- | tailor its decisions to the opponent's cards. Second, we have multiplied the amount of work our AI
-- | needs to do: each "full" decision is an average over tens or hundreds of determinization decisions,
-- | each determinization decision being as difficult as choosing a move in a game of perfect information.

-- | To address these problems, we developed Information Set MCTS (ISMCTS). We keep the powerful idea of randomly
-- | guessing the hidden information but instead of treating each of these guesses as an independent game to
-- | be analysed, we use each guess to guide a single iteration of MCTS. Instead of growing several search
-- | trees and averaging at the end we grow a single tree and essentially average as we go along. For any one
-- | determinization, most moves in the tree will be illegal, and the value of a legal move will have been determined
-- | from maybe hundreds of positions, none of which coincide with the determinization currently being assessed
-- | nor with the actual (unknown) state of the game. This is perhaps a paradoxical way to choose a move, but
-- | works through summing large numbers of imprecise assessments to yield a statistically meaningful result."

-- | The different from my implementation with the above approach is I select one game state, after that ONLY proceed
-- | to determinization phase to randomly generate opponent's hand, this is, of course, for easier implementation. So,
-- | the entire picture of my implementation is I'm going to initialize a tree with one level of possible moves base on
-- | current game state. Then, I'll pass this tree as the initial state ofthe Monte Carlo tree search State Monad, the
-- | State Monad function will then use this initial tree to build a full MCTS tree. The proccess of flows is like below:
-- | select one tree node from the tree using UCT formula,
-- | base on the selected game state (node), randomly generate the rest of cards for "Opponent",
-- | expand one node as the children for the selected node,
-- | check if the expanded node is exists before based on some criteria,
-- | if it doesn't, insert this new node into the tree,
-- | use this node as the beginning game state for the simulation to proccess,
-- | update the simulated result from the newly inserted node all the way back to the root, is proccess is called as back-propagation.
-- | After many rounds, depends on the number input by user in the playCard function, the final card to
-- | choose is the first level node (direct descendant of root) with highest number of simulation performed.
-- | If the depth of searching is too less, it will be no difference with dumb random player.


module Player (
    playCard,
    makeBid
)
where

import Cards
import Deck
import Data.Maybe
import Data.List
import Hearts.Types
import System.Random
import Data.Tree
import Control.Monad.State
import Data.Char

playCard :: PlayFunc
-- | Previous is Nothing means this is first round, just play normally, no need to decide by using MCTS
-- | Example input: "YC" [Card Spade Ace, Card Spade Queen, Card Spade King] [(Card Club Two, "ET")] Nothing
-- | Example output: (Card Spade Ace, "")
playCard _ hand trick Nothing = (cardPicking Nothing trick hand 0 0, newMemo Nothing)
-- | The MCTS function takes in player's ID, current score of this player, number of loops to search and an initialized tree according to this game state
-- | The 3rd parameter of number of loops can change according to user's PC performance
-- | After the searching has done, perform Monad State evaluation to get the card and return to Play.hs
-- | Example input: "YC" 
-- |                map (\ x -> read x :: Card) ["D6","H7","SA","H2","S5","H6","S10","CA","D9","D5","H4","S3","D3","S7","D7","H8","C8","H9","DA","DJ","HJ","SQ","CQ","DK","C4"]
-- |                [(Card Club Jack, "ET")] 
-- |                (Just ([(Card Club King, "ET"),(Card Club Two, "YC")], ""))
-- | Example output: (C8,"CK,C2;ET:0,YC:0,Opponent:0")
playCard pid hand trick previous = (evalState (monteCarloSearch pid (initialScore previous) 100 False) tree, newMemo previous) where
    -- | Function to get the score of this player before starting any tree search
    initialScore :: Maybe ([(Card, PlayerId)], String) -> Int
    initialScore (Just (_, "")) = 0
    initialScore prev = snd $ head $ filter (\ x -> fst x == pid) $ parsePlayersScore prev
    -- | Initialize a tree and pass into MCTS
    tree = treeInitialization pid previous trick hand

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- MEMORY HANDLING PART
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Parsing previous trick to string
-- | Example input: [(Card Club King, "ET"),(Card Club Two, "YC")]
-- | Example output: "CK,C2"
convertTrickToString :: [(Card, PlayerId)] -> String
convertTrickToString l = intercalate "," (map (show . fst) l)

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Updating players' scores sub-part
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Build the 'scoreboard' in the memory when the game is at 2nd trick in a round
-- | Example input: Just ([(Card Club King, "ET"),(Card Club Two, "YC")], "")
-- | Example output: ";ET:0,YC:0"
buildPlayerIndex :: Maybe ([(Card, PlayerId)], String) -> String
buildPlayerIndex Nothing = ""
buildPlayerIndex (Just (previousTrick, _)) = ";" ++ intercalate "," (map (\ x -> snd x ++ ":0") previousTrick)

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Below are the functions to calculate and update players' score to the memory string at 3rd trick and forward
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to check the presence of Queen of Spade from the previous trick
-- | Example input: Just ([(Card Club King, "ET"),(Card Club Two, "YC")], "")
-- | Example output: False
filterBlackMaria :: Maybe ([(Card, PlayerId)], String) -> Bool
filterBlackMaria Nothing = False
filterBlackMaria (Just (prevTrick, _)) = any (\ x -> fst x == Card Spade Queen) prevTrick

-- | Function to filter out the Hearts from the previous trick
-- | Example input: Just ([(Card Heart King, "ET"),(Card Club Two, "YC")], "")
-- | Example output: [(Card Heart King, "ET")]
filterHeartCards :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)]
filterHeartCards Nothing = []
filterHeartCards (Just (prevTrick, _)) = filter (\ x -> suit (fst x) == Heart) prevTrick

-- | Function to find the score of the winner get from previous trick
-- | Example input: Just ([(Card Heart King, "ET"),(Card Club Two, "YC")], "")
-- | Example output: 1
lastTrickScore :: Maybe ([(Card, PlayerId)], String) -> Int
lastTrickScore Nothing = 0
lastTrickScore prevTrick = length (filterHeartCards prevTrick) + blackMaria (filterBlackMaria prevTrick)
    where
        blackMaria :: Bool -> Int
        blackMaria False = 0
        blackMaria True = 13

-- | Function to filter out the cards in previous trick which matches the leading suit to find the winner of the previous trick
-- | Example input: Just ([(Card Heart King, "ET"),(Card Club Two, "YC")], "")
-- | Example output: [(Card Club Two, "YC")]
filterLeadingSuitCards :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)]
filterLeadingSuitCards Nothing = []
filterLeadingSuitCards (Just (prevTrick, _)) = filter (\ x -> suit (fst x) == suit (fst $ last prevTrick)) prevTrick

-- | Function to find the largest binary tuple in terms of their first elements
-- | Source: https://stackoverflow.com/questions/18118280/finding-maximum-element-in-a-list-of-tuples
-- | Example input: [(Card Spade King, "ET"),(Card Club Two, "YC")]
-- | Example output: "YC"
wonPlayer :: Ord a => [(a, PlayerId)] -> PlayerId
wonPlayer [] = []
wonPlayer (x:xs) = snd $ maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | m < fst p = maxTail p ps
          | otherwise   = maxTail (m, n) ps

-- | Simple function to split a string by the given character
-- | Source: https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- | Example input: (==';') "CK,C2;JQ:0,YC:0,Opponent:0"
-- | Example output: ["CK,C2","JQ:0,YC:0,Opponent:0"]
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- | Function to cut down the second half of the memory string and split according to the semicolon character
-- | Example input: Just ([(Card Club Two, "Mark"),(Card Club King, "John")], "Mark:0,John:13")
-- | Example output: ["Mark:0", "John:13"]
getPlayersScoreString :: Maybe ([(Card, PlayerId)], String) -> [String]
getPlayersScoreString Nothing = [""]
getPlayersScoreString (Just (_, memory)) = wordsWhen (==',') (last (wordsWhen (==';') memory))

-- | Function to parse the second half of the memory into a list of tuples which stores all players' scores 
-- | Example input: Just ([(Card Club Two, "Mark"),(Card Club King, "John")], "Mark:0,John:13")
-- | Example output: [("Mark", 0), ("John", 13)]
parsePlayersScore :: Maybe ([(Card, PlayerId)], String) -> [(PlayerId, Int)]
parsePlayersScore Nothing = []
parsePlayersScore (Just (_, "")) = []
parsePlayersScore prev = map ((\ [x, y] -> (x, read y :: Int)) . wordsWhen (==':')) (getPlayersScoreString prev)

-- | Function to return the index position of the winner in the previous trick
-- | Example input: "John" [("Mark", 0), ("John", 13)]
-- | Example output: 1
getWinnerIndex :: PlayerId -> [(PlayerId, Int)] -> Int
getWinnerIndex i memo = getWinner (elemIndex i (map fst memo)) where
    getWinner Nothing = 0
    getWinner (Just a) = a

-- | Function to update the players' scores to the given parsed memory list of tuples
-- | Source: https://stackoverflow.com/questions/15530511/how-to-edit-nth-element-in-a-haskell-list
-- | Example input: "John" 1 [("Mark", 0), ("John", 13)]
-- | Example output: [("Mark", 0), ("John", 14)]
updateScore :: PlayerId -> Int -> [(PlayerId, Int)] -> [(PlayerId, Int)]
updateScore i s memo = update detectShootingMoon where
    list = map (\ x -> (fst x,26)) memo
    n = getWinnerIndex i memo
    lastScore = snd (memo !! n)
    -- | To check whether there is someone who shot the moon at the previous trick
    detectShootingMoon = lastScore + s == 26
    update :: Bool -> [(PlayerId, Int)]
    update moonShot
        | moonShot = take n list ++ [(i,0)] ++ drop (n + 1) list
        | otherwise = take n memo ++ [(i,s + lastScore)] ++ drop (n + 1) memo

-- | Function to update all players score and return a list of tuples
-- | Compiling all the above functions together
-- | Example input: Just ([(Card Club Two, "Mark"),(Card Club King, "John")], "Mark:0,John:13")
-- | Example output: [("Mark", 0), ("John", 14)]
newScore :: Maybe ([(Card, PlayerId)], String) -> [(PlayerId, Int)]
newScore prev = updateScore winner score memo where
    winner = wonPlayer (filterLeadingSuitCards prev)
    memo = parsePlayersScore prev
    score = lastTrickScore prev

-- | Function to convert the list of players' score tuples back to one string
-- | Example input: [("Mark", 0), ("John", 14)]
-- | Example output: "Mark:0,John:14"
updatePlayersScoreToString :: Maybe ([(Card, PlayerId)], String) -> String
updatePlayersScoreToString Nothing = ""
updatePlayersScoreToString prev = intercalate "," (map (\ (x, y) -> x ++ ":" ++ show y) newS) where
    newS = newScore prev

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Test cases: 
-- memory = Just ([(Card Heart Ace, "754"), (Card Heart Queen, "54"), (Card Spade Queen, "234"), (Card Diamond Ten, "256")], "HAHQHKH5H6SASQSKSJD10S10C2;754:10,54:0,234:0,256:16")
-- prev = Just ([(Card Heart Ace, "754"), (Card Heart Queen, "54"), (Card Spade Queen, "234"), (Card Diamond Ten, "256")], "HAHQHKH5H6SASQSKSJD10S10C2;754:10,54:0,234:0,256:16")
-- winner = wonPlayer (filterLeadingSuitCards prev)
-- memo = getPlayersScore prev
-- score = lastTrickScore prev
-- updateScore winner score memo
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Final concatenation
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to concatenate all the parts of the strings return from the above sub-functions into proper updated memory
-- | Example input: (Just ([(Card Club King, "ET"),(Card Club Two, "YC")], ""))
-- | Example output: "CK,C2;ET:0,YC:0,Opponent:0"
newMemo :: Maybe ([(Card, PlayerId)], String) -> String
-- | First trick, no memory to process, thus returning empty string
newMemo Nothing = ""
-- | Second trick, need to build up the "score board", player id of "Opponent" is the id for the simulation player during Monte Carlo Tree Search simulation
newMemo a@(Just (prevTrick, "")) = convertTrickToString prevTrick ++ ";" ++ updatePlayersScoreToString (Just (prevTrick, buildPlayerIndex a)) ++ ",Opponent:0"
-- | Following trick, process the memory as usual
newMemo (Just (prevTrick, previous)) =  convertTrickToString prevTrick ++ "," ++ takeWhile (/= ';') previous ++ ";" ++ updatePlayersScoreToString (Just (prevTrick, previous))

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- CARD DISCARDING PART
-- RANDOM PLAYING WHEN LEADING, A BIT OF HEURISTIC WHEN RENEGING
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | This function is to search for the presence of Heart in the previous trick
-- | Example input: (Just ([(Card Heart King, "ET"),(Card Club Two, "YC")], ""))
-- | Example output: True
findPreviousHeart :: Maybe ([(Card, PlayerId)], String) -> Bool
-- | First round does not have Heart
findPreviousHeart Nothing = False
-- | Find Heart by searching for the character "H" in the memory string and also looking for Heart cards in the previous trick
findPreviousHeart (Just (previousTrick, memory)) = Data.Maybe.isJust (findString "H" memory) || findTrickHeart previousTrick where
    -- | Function to look for the index of the given substring in a string
    -- | Source: https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell
    findString :: (Eq a) => [a] -> [a] -> Maybe Int
    findString search str = findIndex (isPrefixOf search) (tails str)
    -- | Function to check for the presence of Heart in the previous trick by looking for any Heart card
    findTrickHeart :: [(Card, PlayerId)] -> Bool
    findTrickHeart = any (\ x -> suit (fst x) == Heart)

-- | This function is to return a list eligible cards by filtering acccording to the situation
-- | Example input: (Card Club Two) [Card Club Three, Card Spade Ace] Nothing
-- | Example output: [Card Club Three]
renegeAux :: Card -> [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
renegeAux leader hand prev = select (isNothing prev) $ filter (\x -> suit x == suit leader && rank x <= rank leader) hand where
    select :: Bool -> [Card] -> [Card]
    select firstRound leadingSuitCards 
        -- | If there is card which has the same suit with and smaller than the leader, pick from the largest card
        | not (null leadingSuitCards ) = [maximum leadingSuitCards]
        -- | If there is leading suit card but doesn't have any card which is smaller than the leader, pick the largest one from them
        | any (\ x -> suit x == suit leader) hand = [maximum $ filter (\x -> suit x == suit leader) hand]
        -- | If it is the first trick and there's no Club on the hand, pick from other cards which are not Hearts or Queen of Spade
        -- | Second parameter of False is just a dummy value, the recursion will go to first condition anyway
        -- | Third parameter of recursive call is replacing the leading suit to non-Heart and not Queen of Spade
        | firstRound = select False $ filter (\ x -> suit x /= Heart && x /= Card Spade Queen) hand
        -- | Otherwise, if there is no card which has the same suit with the leader, pick the card with highest rank from Hearts or Queen of Spade
        -- | If still filter out nothing (no Heart or Queen of Spade left), pick otherwise (back to the third condition, i.e. not leading suit, not Heart and not Queen of Spade)
        | otherwise = [maximum $ select True $ filter (\ x -> suit x == Heart || x == Card Spade Queen) hand]

-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
-- | Example input: [Card Club Three, Card Spade Ace] (Just ([(Card Heart King, "ET"),(Card Club Two, "YC")], ""))
-- | Example output: [Card Club Three, Card Spade Ace]
leadAux :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
leadAux hand previous = select (findPreviousHeart previous) (filter (\x -> suit x /= Heart) hand) (find (== Card Club Two) hand) where
    select :: Bool -> [Card] -> Maybe Card  -> [Card]
    select hBreak nonHeartList clubTwo
        -- | If I have Two of Club
        | isJust clubTwo = [fromJust clubTwo]
        -- | If the Heart didn't break and not only Heart on the hand
        | not hBreak && not (null nonHeartList) = nonHeartList
        -- | If the Heart is broken or there's only Heart on the hand, pick whatever from the hand
        | otherwise = hand

-- | Function to pick a card from the eligible card list by looking at the situation
-- | Example input: (Just ([(Card Club King, "ET"),(Card Club Two, "YC")], ""))
-- |                [(Card Club Jack, "ET")]
-- |                map (\ x -> read x :: Card) ["D6","H7","SA","H2","S5","H6","S10","CA","D9","D5","H4","S3","D3","S7","D7","H8","C8","H9","DA","DJ","HJ","SQ","CQ","DK","C4"]
-- |                0
-- |                0
-- | Example output: Card Club Eight
cardPicking :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> [Card] -> Int -> Int -> Card
cardPicking previous currentTrick hand tw ts 
    -- | Current trick is null means I have to lead
    | not (null currentTrick) = pick renege 
    -- | Current trick is not empty means I have to renege
    | otherwise = pick lead where
        renege = renegeAux (fst $ last currentTrick) hand previous
        lead = leadAux hand previous
        pick :: [Card] -> Card
        pick l = l !! simRandom l (map fst currentTrick) tw ts

-- | Function to return random index number
-- | Example input: map (\ x -> read x :: Card) ["D6","H7","SA","H2","S5","H6","S10","CA","D9","D5","H4","S3","D3","S7","D7","H8","C8","H9","DA","DJ","HJ","SQ","CQ","DK","C4"]
-- | Example output: 5
getRandomIndex :: [Card] -> Int
getRandomIndex l = fst (randomR (0, length l - 1) (mkStdGen $ seed l))

-- | Function to return a seed value based on the hand of cards given using some hash-function-like formula
seed :: [Card] -> Int
-- | Example input: map (\ x -> read x :: Card) ["D6","H7","SA","H2","S5","H6","S10","CA","D9","D5","H4","S3","D3","S7","D7","H8","C8","H9","DA","DJ","HJ","SQ","CQ","DK","C4"]
-- | Example output: 1731889960702219230
seed l = extractNumber (cardToString l) * 12345 where
    -- | Convert all the card objects into one string
    cardToString :: [Card] -> String
    cardToString = concatMap show

-- | Function to concatenate all of the numeric characters and convert to integer
-- | Example input: "D6H7SAH2S5H6S10CAD9D5H4S3D3S7D7H8C8H9DADJHJSQCQDKC4"
-- | Example output: 672561095433778894
extractNumber :: String -> Int
extractNumber xs = readNum $ filter isDigit xs where
    -- | Function to convert the resulting string of number into integer type
    readNum :: String -> Int
    readNum [] = 0
    readNum r = read r :: Int

-- | Random number generator
-- | Example input: (map (\ x -> read x :: Card) ["D6","H7","SA","H2","S5","H6","S10","CA","D9","D5","H4","S3","D3","S7","D7","H8","C8","H9","DA","DJ","HJ","SQ","CQ","DK","C4"]) [Card Club Jack] 0 0
-- | Example output: 14
simRandom :: [Card] -> [Card] -> Int -> Int -> Int
simRandom l currentTrick tw ts = abs ((getRandomIndex currentTrick + 1) * getRandomIndex l * simFactor) `mod` length l where
    simFactor = fst $ random generator
    -- | 2n+1 to explicitly make the input into odd number because mkStdGen doesn't work for even number
    generator = mkStdGen ((2 * (tw + ts + 1234567) + 1 :: Int) ^ (12345 :: Int) :: Int)

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- MONTE CARLO TREE SEARCH NODE DATA TYPE DECLARATION
-- this is the data type which will be used during MCTS
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

data MCTSNode = MCTSNode {
    -- | Attribute to store the memory
    prevTrickMemo :: Maybe ([(Card, PlayerId)], String), 
    -- | Attribute to store current trick
    trickSoFar :: [(Card, PlayerId)], 
    -- | Attribute to store the hand of the player
    hand:: [Card], 
    -- | Winner ID of the player who won previous trick
    winnerId :: PlayerId, 
    -- | Times of the simulation which leads to winning
    timesWon :: Int, 
    -- | Total times of simulation for grand total of all its successor nodes
    timesSimulated :: Int, 
    -- | Record down the total number of times its parent's number of times simulated
    parentTimesSimulated :: Int, 
    -- | Turn of myself
    self :: Bool
} deriving (Show)

instance Eq MCTSNode where
    -- | Compare the nodes on the criteria of memory, trick, hand and turn of myself
    MCTSNode{prevTrickMemo = p, trickSoFar = c, hand = h, self = s} == MCTSNode{prevTrickMemo = p', trickSoFar = c', hand = h', self = s'} = (p == p') && (c == c') && (h == h') && (s == s')

instance Ord MCTSNode where
    -- | Compare the order of nodes by number of times simulated
    compare MCTSNode{timesSimulated = ts} MCTSNode{timesSimulated = ts'} = compare ts ts'

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- TREE BUILDING PART
-- THIS PART OF CODES IS TO INITIALIZE A TREE FOR THE MCTS TO RUN
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to generate a tree with a root node (current game situation) and also all of its children (all possible moves from current game state)
treeInitialization :: PlayerId -> Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> [Card] -> Tree MCTSNode
treeInitialization pid prev current hand = Node (MCTSNode prev current hand (wonPlayer $ filterLeadingSuitCards prev) 0 noOfEligibleCards noOfEligibleCards True) $ children full where
    -- | To get the eligible cards based on current game state
    eligibleCards :: [(Card, PlayerId)] -> [Card]
    eligibleCards [] = leadAux hand prev
    eligibleCards c = renegeAux (fst $ last c) hand prev
    -- | Generate the first layer children of the tree
    children :: Bool -> Forest MCTSNode
    children lastPlayerOfTrick
        -- | If the player is the last player of the trick, refresh the children node to a new trick, 
        -- | Pass the current trick into previous trick, 
        -- | Concatenate the previous trick into the memory and 
        -- | Calculate the accumulated players' scores in second half of memory
        | lastPlayerOfTrick = map (\ x -> Node (MCTSNode (Just ((x, pid) : current, newMemo prev)) [] (delete x hand) (wonPlayer $ filterLeadingSuitCards (Just ((x, pid) : current, newMemo prev))) 0 1 noOfEligibleCards False) []) $ eligibleCards current
        -- | Otherwise generate all the children node normally by adding one card to the current trick
        | otherwise = map (\ x -> Node (MCTSNode prev ((x, pid) : current) (delete x hand) (wonPlayer $ filterLeadingSuitCards prev) 0 1 noOfEligibleCards False) []) $ eligibleCards current
    noOfEligibleCards = length $ eligibleCards current
    -- | Check is the player is the last player
    full = length current == getNumberOfOpponent prev hand

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- MONTE CARLO TREE SEARCH PART
-- THIS BIG PORTION OF CODES IS TO PERFORM MONTE CARLO TREE SEARCH
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | THIS SUB-PART OF CODES PERFORMS THE SELECTION PHASE OF THE TREE SEARCH
-- | Selection: start from root R and select successive child nodes until a leaf node L is reached. The root is the current game state and a leaf is any node from which 
-- | no simulation (playout) has yet been initiated. The section below says more about a way of biasing choice of child nodes that lets the game tree expand towards the 
-- | most promising moves, which is the essence of Monte Carlo tree search. -- Wikipedia
-- | link: https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to get all nodes from the tree which has the highest UCT score
-- | The nodes with highest score is not necessary to be only one, that's why the returning type is a list
getNode :: Tree MCTSNode -> [MCTSNode]
getNode tree = foldr (\ x a -> if ucbScore x then x : a else a) [] tree where
    -- | UCT (Upper Confidence Bound 1 applied to trees) formula
    -- | UCT is based on the UCB1 formula derived by Auer, Cesa-Bianchi, and Fischer[38] and the provably convergent AMS (Adaptive Multi-stage Sampling) algorithm 
    -- | First applied to multi-stage decision making models (specifically, Markov Decision Processes) by Chang, Fu, Hu, and Marcus.[11] Kocsis and Szepesvári 
    -- | Recommend to choose in each node of the game tree the move for which the expression below has the highest value. -- Wikipedia
    -- | In this formula:
    -- | w stands for the number of wins for the node considered after the i-th move
    -- | n stands for the number of simulations for the node considered after the i-th move
    -- | np stands for the total number of simulations after the i-th move ran by the parent node of the one considered
    -- | c is the exploration parameter—theoretically equal to √2; in practice usually chosen empirically, I chose sqrt 2 in this implementation
    ucb :: Floating a => a -> a -> a -> a
    ucb w n np = (w / n) + (sqrt 2 * sqrt (log np / n))
    -- | Function to return the highest UCT score among all of the nodes in the tree
    maxUCB :: Tree MCTSNode -> Double
    maxUCB tree' = maximum ((\ x -> ucb (fromIntegral $ timesWon x) (fromIntegral $ timesSimulated x ) (fromIntegral $ parentTimesSimulated x) :: Double) <$> tree')
    maxS = maxUCB tree
    ucbScore x = ucb (fromIntegral $ timesWon x) (fromIntegral $ timesSimulated x ) (fromIntegral $ parentTimesSimulated x) == maxS

-- | Function to retrieve the node with highest UCT score
selection :: Tree MCTSNode -> MCTSNode
selection tree = head $ getNode tree

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | DETERMINIZATION PHASE OF THE MCTS BEFORE EXPANSION PHASE
-- | link: http://www.aifactory.co.uk/newsletter/2013_01_reduce_burden.htm
-- | This is required as this MCTS is applying to an imperfect information game -- Hearts
-- | to randomly generate the hands for opponents, in the case of against 3 players (4 players mode), integrate 3 players into one opponent for easier simulation
-- | the game is de facto perfect information game if is playing in 1v1 mode, theoritically MCTS can perform better in this case
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to shuffle a pile of cards, the working principle is totally the same as the shuffle function defind in Deck.hs
-- | Only different is I changed the seed from randomIO to another seed created by myself
shuffle :: [Card] -> [Card]
shuffle l = map snd $ sortOn fst $ zip i l where
    i = take (length l) (randoms (mkStdGen (seed l)) :: [Double])

-- | Function to filter a list base by another list, i.e. remove all the elements which is present in another list
-- | Source: https://stackoverflow.com/questions/33017435/how-to-filter-a-list-by-another-list-in-haskell
eliminate :: Ord a => [a] -> [a] -> [a]
eliminate _ [] = []
eliminate [] _ = []
eliminate (x:xs) l
    | x `notElem` l = x : eliminate xs l
    | otherwise = eliminate xs l

-- | Function to generate all the cards which have been used
generateUsedCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
generateUsedCards Nothing = []
-- | If it is the second round, memory is empty, the only cards used is from the previous trick (first round)
generateUsedCards (Just (prev, "")) = map fst prev
-- | Otherwise get all the cards by parsing the memory string and concatenate with previous trick
generateUsedCards (Just (prev, memo)) = map fst prev ++ map (\ x -> read x :: Card) (wordsWhen (==',') (takeWhile (/= ';') memo))

-- | Function to generate all remaining cards based on the game states in the MCTS tree, returning the shuffled card pile after filtering from deck of 52 cards
opponentCards :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> [Card] -> [Card]
opponentCards prev current hand = shuffle (eliminate sortedDeck (generateUsedCards prev ++ hand ++ map fst current))

-- | Main function of determinization function, returning all shuffled cards which are remaining in this game (haven't been discarded)
determinization :: PlayerId -> [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, String)], String) -> [Card]
determinization _ hand currentTrick prev = opponentCards prev currentTrick hand

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | EXPANSION PHASE
-- | Expansion: unless L ends the game decisively (e.g. win/loss/draw) for either player, create one (or more) child nodes and choose node C from one of them. 
-- | Child nodes are any valid moves from the game position defined by L. -- Wikipedia
-- | link: https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to return the number of opponents in the game according to the length of previous trick
getNumberOfOpponent :: Maybe ([(Card, PlayerId)], String) -> [Card] -> Int
getNumberOfOpponent (Just (p, _)) _ = length p - 1
-- | Although this condition is not using, but better to set it as 0 if it is the first round
getNumberOfOpponent Nothing _ = 0

-- | Function to expand a new child for the selected node, however this newly produced node is not finalized yet, because the trick is not getting updated if it is full (the round has finished)
expand :: PlayerId -> Tree MCTSNode -> MCTSNode -> [Card] -> (MCTSNode, [Card])
expand selfId _ (MCTSNode prev trick selfHand winnerId totalWinningTimes totalSimulatedTimes _ self) oppCards
    -- | If the expanding node is my turn, pick a card normally by using renege or leading strategy, set the next turn to AI
    | self = (MCTSNode prev (discardMyself ++ trick) (eliminate selfHand (map fst discardMyself)) winnerId 0 0 totalSimulatedTimes False, oppCards)
    -- | If the trick is empty and not my turn (AI is leading), pick a random number of cards once together, then set the next turn to my turn (True, 8th attribute of MCTSNode)
    | null trick = (MCTSNode prev randomNumberOfNewCards selfHand winnerId 0 0 totalSimulatedTimes True, eliminate oppCards (map fst randomNumberOfNewCards))
    -- | If I have only one opponent and he's not leading, renege a card from HIS hand, set next turn to me
    | getNumberOfOpponent prev selfHand == 1 = (MCTSNode prev (discardOneVOneOpp ++ trick) selfHand winnerId 0 0 totalSimulatedTimes False, eliminate oppCards (map fst discardOneVOneOpp))
    -- | If the opponent is not only one person and is their turn, pick cards until the end of the trick, temporarily set the next turn to themselves
    | otherwise = (MCTSNode prev (discardOppRemainingCards ++ trick) selfHand winnerId 0 0 totalSimulatedTimes False, eliminate oppCards (map fst discardOppRemainingCards))
        where
            discardMyself = discard selfHand selfId
            discardOppRemainingCards = newCards spaceLeft oppCards
            discardOneVOneOpp = discard oppCards "Opponent"
            spaceLeft = getNumberOfOpponent prev selfHand - length trick + 1
            randomNumberOfNewCards = newCards (randomNumber 0 `mod` getNumberOfOpponent prev selfHand) (delete (fst $ head $ discard oppCards "Opponent") oppCards) ++ discard oppCards "Opponent"
            -- | Function to return a random number + a counter to increment the index value, avoiding the system to pick same cards throughout the recursion
            randomNumber :: Int -> Int
            randomNumber n = simRandom oppCards (map fst trick) totalWinningTimes totalSimulatedTimes + n
            -- | Discard function to pick a card for either myself or the ONE opponent
            discard :: [Card] -> PlayerId -> [(Card, PlayerId)]
            discard cards pid = [(cardPicking prev trick cards totalWinningTimes totalSimulatedTimes, pid)]
            -- | This function is to pick a number of cards from the given hand
            newCards :: Int -> [Card] -> [(Card, PlayerId)]
            newCards 0 _ = []
            newCards n cards = (cards !! (randomNumber n `mod` length cards), "Opponent") : newCards (n - 1) cards

-- | Function to check if the trick is full
fullTrick :: (MCTSNode, [Card]) -> Int -> Bool
fullTrick (MCTSNode _ trick _ _ _ _ _ _, _) numberOfPlayer = length trick == numberOfPlayer

-- | Function to refresh the expanded node if it needs to refresh (if the trick in the expanded node is full)
initialState :: (MCTSNode, [Card]) -> Bool -> PlayerId -> (MCTSNode, [Card])
-- | If no need to refresh, do nothing
initialState a False _ = a
-- | If needs to refresh, replace previous trick with current trick and produce a new memory string by using newMemo function above, make the trick so far to empty
initialState (MCTSNode p c h _ tw ts pts _, l) True selfId = (MCTSNode (Just (c, newMemo p)) [] h winId tw ts pts (winId == selfId), l) where
    winId = wonPlayer (filterLeadingSuitCards (Just (c, "")))

-- | Function to check is the expanded node is already present in the tree
nodePresent :: (MCTSNode,[Card]) -> Tree MCTSNode -> Bool
nodePresent q tree = fst q `elem` tree

-- | Retrieve the node if the node is already present in the tree in terms of previous tricks, current trick and hand of myself
-- | to avoid node duplication in the tree
retrieveNodeIfPresent :: (MCTSNode, [Card]) -> Bool -> Tree MCTSNode -> (MCTSNode, [Card])
retrieveNodeIfPresent (ini, l) True tree = (head $ filter (== ini) (flatten tree), l)
retrieveNodeIfPresent a False _ = a

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | SIMULATION PHASE
-- | Simulation: complete one random playout from node C. This step is sometimes also called playout or rollout. A playout may be as simple as choosing uniform random moves 
-- | until the game is decided (for example in chess, the game is won, lost, or drawn). -- Wikipedia
-- | link: https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to refresh the game state by storing current trick to previous trick and store previous trick to string
-- | will not update the game state if the trick is not full yet
refresh :: Bool -> (MCTSNode, [Card]) -> PlayerId -> PlayerId -> (MCTSNode, [Card])
refresh True (q, p) wp i = (MCTSNode (Just (trickSoFar q, newMemo (prevTrickMemo q))) [] (hand q) wp (timesWon q)(timesSimulated q) (parentTimesSimulated q) (wp == i), p)
refresh False a _ _ = a

-- | The main simulation program of the MCTS algorithm, this function will take a game state as the initial state and run the simulation of the game
-- | by playing against itself using random strategy
simulation :: PlayerId -> Bool -> State (MCTSNode, [Card]) (PlayerId, Int)
-- | Second parameter of True means the loop has ended, instead of keep doing the simulation, it evaluates the final score of the player (not the virtual OPPONENT AI)
simulation i True = do
    -- | Get the final game state
    (s, _) <- get
    let
        -- | Evaluate the final scores of all players, including current player (the player who is passed from playCard function) and filter out its id to get its score
        final = filter (\ x -> fst x == i) (newScore (prevTrickMemo s))
    -- | Return the score
    return $ head final
simulation i False = do
    -- | Retrieve the game states
    -- | s refers to the game state, c refers to the cards held by virtual opponent 
    (s, c) <- get
    let
        -- | Base on the given game state, extend a new game state, logic same as expansion phase, so I just reuse the expand function
        newState = expand i (Node s []) s c
        -- | Get the possible player who is "currently" winning the trick, although the trick of newState is not ended yet
        -- | But this variable will only be used in the refresh function if the trick is full
        -- | Otherwise it will not be used at all (if the trick is not full, the "winner" is not the de facto winner), the refresh function will just ignore this argument
        wp = wonPlayer (filterLeadingSuitCards (Just (trickSoFar (fst newState), "")))
        -- | Get the number of player
        noOfPlayer = getNumberOfOpponent (prevTrickMemo (fst newState)) (hand (fst newState)) + 1
        -- | Refresh the game state if the trick is full, produce an empty trick and store previous trick to memory
        -- | The game state will not get updated if the trick is not full
        nextState = refresh (length (trickSoFar (fst newState)) == noOfPlayer) newState wp i
        -- | To check if it is the last loop of simulation by checking the number of cards on my hand and virtual opponent hand
        -- | If both are null, pass False to the recursive function call
        lastRound = null (snd newState) && null (hand $ fst newState)
    -- | Put the newly generated game state into the State Monad
    put nextState
    -- return ("", 0)
    -- | Loop back to itself
    simulation i lastRound

-- | Function to evaluate the player is winning or losing the game in the simulation
winLose :: (PlayerId, Int) -> Int -> Bool
-- | Initial score is the score when calling playCard function
-- | Initial score + 3 is the maximum score which the AI can take in one game
winLose (_, score) initialScore = score == 0 || score <= initialScore + 3
-- winLose (pid, score) initialScore = score <= 12
-- winLose (_, score) initialScore = score <= initialScore + ((26 - initialScore) `div` 4)

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BACKPROPAGATION PHASE
-- Backpropagation: use the result of the playout to update information in the nodes on the path from C to R. -- Wikipedia
-- link: https://en.wikipedia.org/wiki/Monte_Carlo_tree_search
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to insert the finalized new node or don't do anything if the node does exists in the tree
insertNode :: MCTSNode -> (MCTSNode, [Card]) -> Tree MCTSNode -> Bool -> Tree MCTSNode
-- | If the node is present in the tree, return the same tree
insertNode _ _ a@(Node _ _) True = a
-- | If not, use recursion to "insert" a new node
insertNode se ex (Node x xs) False 
    -- | If current node is the parent of the new node, concat it to it's children list
    | x == se = Node x (Node (fst ex) [] : xs)
    -- | Otherwise keep recur until can find the matched parent node
    | otherwise = Node x (map (\ y -> insertNode se ex y False) xs)

-- | Main function to perform back propagation, update the win rate probability all the way from new node to root
backPropagation :: (MCTSNode, [Card]) -> Bool -> Tree MCTSNode ->  Tree MCTSNode
backPropagation ex wl a@(Node (MCTSNode p c h w tw ts pts self) xs) = Node (newX wl present) (map (updateParentSimulation . backPropagation ex wl) xs) where
    -- | Variable to store the Boolean to indicate whether this node is present before in the tree
    present = nodePresent ex a
    -- | Function to update nodes with new probabilistic values
    newX :: Bool -> Bool -> MCTSNode
    newX win True = MCTSNode p c h w (ntw win) (nts True) (pts + 1) self
    newX _ False = MCTSNode p c h w tw ts (pts + 1) self
    -- | Function to increment total simulated times if current node has a successor which is the simulated node
    nts :: Bool -> Int
    nts True = ts + 1
    nts False = ts
    -- | Function to increment total won times if current node has a successor which is the simulated node
    ntw :: Bool -> Int
    ntw True = tw + 1
    ntw False = tw
    -- | Function to update the number of times the total number of simulation by all of its successors
    -- | update all its children data by mapping throughout the forest (list of trees)
    -- | plus one if there is the node
    updateParentSimulation :: Tree MCTSNode -> Tree MCTSNode
    updateParentSimulation (Node (MCTSNode p' c' h' w' tw' ts' _ self') xs') = Node (MCTSNode p' c' h' w' tw' ts' (nts present) self') xs'

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- GRAND COMPILATION OF ALL OF THE SUB-PARTS OF THE ABOVE MCTS FUNCTIONS
-- this is where the Monte Carlo Search Tree algorithm will run
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Function to read previous trick in case of current trick is empty
checkPrevRound :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
-- | Retrive the cards in the previous trick
checkPrevRound [] (Just (p, _)) = map fst p
-- | If current trick is not empty, return current trick
checkPrevRound c _ = c

-- | Function to evaluate which card to choose in this game
evaluation :: State (Tree MCTSNode) Card
evaluation = do
    tree <- get
    let 
        -- | Find the direct children node from root which has the highest times of simulation
        maximumNode = maximum (levels tree !! 1)
        -- | Get the card in which the node has chosen by reading from the trick 
        currentCards = map fst (trickSoFar maximumNode)
        -- | Read from previous trick in case current trick is empty
        finalCards = checkPrevRound currentCards (prevTrickMemo maximumNode)
    -- | Return the card from the trick
    return $ head finalCards

-- | Main part of Monte Carlo Tree Search
-- | This Monad state function performs the simulation of Monte Carlo Tree Search
-- | Second parameter is the player's score before performing Monte Carlo Tree Search
-- | Third parameter is the loop counter, this decides how deep the tree can go into
-- | Fourth Boolean parameter is to end the MCTS loop if one of the leaves (game states) already reaches the end of the game (NOT IN THE SIMULATION PHASE)
monteCarloSearch :: PlayerId -> Int -> Int -> Bool -> State (Tree MCTSNode) Card
-- | If the loop counter already decreased to 0, end the loop
monteCarloSearch _ _ 0 _ = evaluation
-- | If the game state already reaches the end of the game (Boolean), end the loop
monteCarloSearch _ _ _ True = evaluation
-- | Otherwise, continue perform the MCTS process throughout 4 phases, which are:
-- | Selection
-- | Expansion
-- | Simulation
-- | Back-propagation
monteCarloSearch pid initialScore n _ = do
    -- | Get the tree Monad instance (game states)
    tree <- get
    let 
        -- | Select a node from the tree to perform the search
        se = selection tree
        -- | Randomly generate the other players' card
        -- | Combine all 3 players' cards into one big pile of cards for easier game simulation
        de = determinization pid (hand se) (trickSoFar se) (prevTrickMemo se)
        -- | Expand a new node for the selected tree node (as a new child for the selected node)
        ex = expand pid tree se de
        -- | Check is the trick so far in the new expanded game state is full, i.e. 2 or 4 players
        ft = fullTrick ex (getNumberOfOpponent (prevTrickMemo se) (hand se) + 1)
        -- | Get an initial state based on the expanded node (performing refreshing if needed for the expanded node)
        ini = initialState ex ft pid
        -- | Check if the node is present in the tree before
        bo = nodePresent ini tree
        -- | If yes, get the node from the tree (because the node includes the win and simulation counts, if not they will always be 0)
        -- | If doesn't exits, just use the inital state expanded from the selection phase
        ini' = retrieveNodeIfPresent ini bo tree
        -- | Perform simulation based on the given initial state to the end of game
        sim = evalState (simulation pid False) ini'
        -- | Return a Boolean to tell if the player is win or lose in the simulation above
        wl = winLose sim initialScore
        -- | A temperary varaible to store the tree with inserted (expanded) new node
        temp = insertNode se ini' tree bo
        -- | Update the tree based on the result returned
        newTree = backPropagation ini' wl temp
    -- | Put the new tree into the State Monad
    put newTree
    -- | Perform another MCTS using the new tree
    -- | Decrement the counter
    -- | Check if the hand in the initial state expanded is less than 2 cards
        -- | If so, the final result is most probably fixed, end the loop
        -- | If no, continue the loop
    monteCarloSearch pid initialScore (n - 1) (length (hand $ fst ini') < 2)

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- UTILITIES
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- | Given a card, select its rank.
rank :: Card -> Rank
rank (Card _ r) = r

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined

-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- TEST CASES
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- prevM = Just ([(Card Club King, "JQ"),(Card Club Two, "YC")], "")
-- prevM = Nothing
-- current' = [(Card Club Jack, "JQ")]
-- current' = []
-- hand' = map (\ x -> read x :: Card) ["D6","H7","SA","H2","S5","H6","S10","CA","D9","D5","H4","S3","D3","S7","D7","H8","C8","H9","DA","DJ","HJ","SQ","CQ","DK","C4"]
-- hand' = [Card Diamond Six, Card Heart Seven, Card Spade Ace, Card Heart Two, Card Spade Five]
-- test' :: Tree MCTSNode
-- test = Node (MCTSNode prevM current' hand' "2" 2 4 4 True) [Node (MCTSNode prevM [(Card Diamond Five, "3"), (Card Diamond Two, "1"), (Card Diamond Ace, "2")] [Card Heart Six,Card Heart King,Card Spade Ace,Card Club King,Card Diamond Jack,Card Club Three,Card Heart Four,Card Club Queen] "2" 0 1 4 False) [], Node (MCTSNode prevM [(Card Diamond Jack, "3"), (Card Diamond Two, "1"), (Card Diamond Ace, "2")] [Card Heart Six,Card Diamond Five,Card Heart King,Card Spade Ace,Card Club King,Card Club Three,Card Heart Four,Card Club Queen] "2" 2 2 4 False) [Node (MCTSNode (Just ([(Card Diamond Eight, "Opponent"), (Card Diamond Jack, "3"), (Card Diamond Two, "1"), (Card Diamond Ace, "2")], "DK,D4,D3,D7,C2,CA,C4,C9,S9,S2,S10,SQ,S8,S3,SK,S7;1:0,2:0,3:13,4:0")) [] [Card Heart Six,Card Diamond Five,Card Heart King,Card Spade Ace,Card Club King,Card Club Three,Card Heart Four,Card Club Queen] "3" 1 1 2 True) []]]
-- test = Node (MCTSNode prevM current' hand' "2" 0 2 2 True) [Node (MCTSNode prevM ((Card Diamond Five, "3") : current') (delete (Card Diamond Five) hand') "2" 0 1 2 False) [], Node (MCTSNode prevM ((Card Diamond Jack, "3") : current') (delete (Card Diamond Jack) hand') "2" 0 1 2 False) []]
-- test' = Node (MCTSNode prevM current' hand' "2" 0 3 3 True) [Node (MCTSNode prevM ((Card Heart Six, "3") : current') (delete (Card Heart Six) hand') "2" 0 1 3 False) [], Node (MCTSNode prevM ((Card Heart King, "3") : current') (delete (Card Heart King) hand') "2" 0 1 3 False) [], Node (MCTSNode prevM ((Card Heart Four, "3") : current') (delete (Card Heart Four) hand') "2" 0 1 3 False) []]
