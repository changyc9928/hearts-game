-- | Write a report describing your design and strategy here.
module Player_copy (
    playCard,
    makeBid
)
where
-- You can add more imports as you need them.
import Cards
import Deck
import Data.Maybe
import Data.List
import Hearts.Types
import System.Random
import Data.Tree
import Control.Monad.State
import Data.Tuple

-- Nicholas: You need to read Types.hs and Cards.hs at minimum to start doing things.
-- Play.hs contains the sequence of play for every round, but it's not necessary to fully understand it.
-- You only need to know how the game works, and you just need to play a single card, everything else is automated.
-- The problem is choosing the right card to play. This is what you'll need to solve.

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] _ = (lead hand Nothing, "")
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick previous = (renege (suit $ fst $ last trick) hand trick previous, newMemo Nothing)
-- | The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- playCard pid hand trick previous = (evalState (monteCarloSearch pid (initialScore previous) 50 False) tree, newMemo previous) where
--     initialScore (Just (_, "")) = 0
--     initialScore prev = snd $ head $ filter (\ x -> fst x == pid) $ getPlayersScore prev
--     tree = treeInitialization pid previous trick hand

-- Source: https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

filterHeartCards :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)]
filterHeartCards Nothing = []
filterHeartCards (Just (prevTrick, _)) = filter (\ x -> suit (fst x) == Heart) prevTrick

filterBlackMaria :: Maybe ([(Card, PlayerId)], String) -> Bool
filterBlackMaria Nothing = False
filterBlackMaria (Just (prevTrick, _)) = any (\ x -> fst x == Card Spade Queen) prevTrick

-- Test cards: prevTrick = Just ([(Card Heart Ace, "754"), (Card Heart Queen, "54"), (Card Spade Queen, "234"), (Card Diamond Ten, "256")], "")
lastTrickScore :: Maybe ([(Card, PlayerId)], String) -> Int
lastTrickScore Nothing = 0
-- lastTrickScore prevTrick = wonPlayer (filterLeadingSuitCards prevTrick) ++ ":" ++ show (length (filterHeartCards prevTrick) + blackMaria (filterBlackMaria prevTrick))
lastTrickScore prevTrick = length (filterHeartCards prevTrick) + blackMaria (filterBlackMaria prevTrick)
    where
        blackMaria :: Bool -> Int
        blackMaria False = 0
        blackMaria True = 13

filterLeadingSuitCards :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)]
filterLeadingSuitCards Nothing = []
filterLeadingSuitCards (Just (prevTrick, _)) = filter (\ x -> suit (fst x) == suit (fst $ last prevTrick)) prevTrick

-- Test case: memory = Just ([(Card Heart Ace, "754"), (Card Heart Queen, "54"), (Card Spade Queen, "234"), (Card Diamond Ten, "256")], "HAHQHKH5H6SASQSKSJD10S10C2;754:10,54:0,234:0,256:16")
getPlayersScoreString :: Maybe ([(Card, PlayerId)], String) -> [String]
getPlayersScoreString Nothing = [""]
getPlayersScoreString (Just (_, memory)) = wordsWhen (==',') (last (wordsWhen (==';') memory))

getPlayersScore :: Maybe ([(Card, PlayerId)], String) -> [(PlayerId, Int)]
getPlayersScore Nothing = []
getPlayersScore (Just (_, "")) = []
getPlayersScore prev = map ((\ [x, y] -> (x, read y :: Int)) . wordsWhen (==':')) (getPlayersScoreString prev)

getWinnerIndex :: PlayerId -> [(PlayerId, Int)] -> Int
getWinnerIndex i memo = getWinner (elemIndex i (map fst memo)) where
    getWinner Nothing = 0
    getWinner (Just a) = a

-- prev = Just ([(Card Heart Ace, "754"), (Card Heart Queen, "54"), (Card Spade Queen, "234"), (Card Diamond Ten, "256")], "HAHQHKH5H6SASQSKSJD10S10C2;754:10,54:0,234:0,256:16")
-- winner = wonPlayer (filterLeadingSuitCards prev)
-- memo = getPlayersScore prev
-- score = lastTrickScore prev
-- updateScore winner score memo
-- Source: https://stackoverflow.com/questions/15530511/how-to-edit-nth-element-in-a-haskell-list
updateScore :: PlayerId -> Int -> [(PlayerId, Int)] -> Bool -> [(PlayerId, Int)]
updateScore i _ memo True = take n list ++ [(i,0)] ++ drop (n + 1) list where
    list = map (\ x -> (fst x,26)) memo
    n = getWinnerIndex i memo
updateScore i s memo False = take n memo ++ [(i,s + lastScore)] ++ drop (n + 1) memo where
    n = getWinnerIndex i memo
    lastScore = snd (memo !! n)

-- prev = Just ([(Card Club Ace, "754"), (Card Diamond Nine, "54"), (Card Spade Queen, "234"), (Card Diamond Ten, "256")], "HA,HQ,HK,H5,H6,SA,SQ,SK,SJ,D10,S10,C2;754:10,54:0,234:0,256:13")
detectShootingMoon :: PlayerId -> Int -> [(PlayerId, Int)] -> Bool
detectShootingMoon winner score memo = lastScore + score == 26 where
    n = getWinnerIndex winner memo
    lastScore = snd (memo !! n)

newScore :: Maybe ([(Card, PlayerId)], String) -> [(PlayerId, Int)]
newScore prev = updateScore winner score memo shoot where
    winner = wonPlayer (filterLeadingSuitCards prev)
    memo = getPlayersScore prev
    score = lastTrickScore prev
    shoot = detectShootingMoon winner score memo

convertToString :: Maybe ([(Card, PlayerId)], String) -> String
convertToString Nothing = ""
convertToString prev = intercalate "," (map (\ (x, y) -> x ++ ":" ++ show y) newS) where
    newS = newScore prev

-- Source: https://stackoverflow.com/questions/18118280/finding-maximum-element-in-a-list-of-tuples
wonPlayer :: Ord a => [(a, PlayerId)] -> PlayerId
wonPlayer [] = []
wonPlayer (x:xs) = snd $ maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | m < fst p = maxTail p ps
          | otherwise   = maxTail (m, n) ps

buildPlayerIndex :: Maybe ([(Card, PlayerId)], String) -> String
buildPlayerIndex Nothing = ""
buildPlayerIndex (Just (previousTrick, _)) = ";" ++ intercalate "," (map (\ x -> snd x ++ ":0") previousTrick)

-- Source: https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

convertTrickToString :: [(Card, PlayerId)] -> String
convertTrickToString l = intercalate "," (map (show . fst) l)

findPreviousHeart :: Maybe ([(Card, PlayerId)], String) -> Bool
findPreviousHeart Nothing = False
findPreviousHeart (Just (previousTrick, memory)) = Data.Maybe.isJust (findString "H" memory) || findTrickHeart previousTrick

findTrickHeart :: [(Card, PlayerId)] -> Bool
findTrickHeart = any (\ x -> suit (fst x) == Heart)

newMemo :: Maybe ([(Card, PlayerId)], String) -> String
newMemo Nothing = ""
newMemo a@(Just (prevTrick, "")) = convertTrickToString prevTrick ++ ";" ++ convertToString (Just (prevTrick, buildPlayerIndex a)) ++ ",Opponent:0"
newMemo (Just (prevTrick, previous)) =  convertTrickToString prevTrick ++ "," ++ takeWhile (/= ';') previous ++ ";" ++ convertToString (Just (prevTrick, previous))

seed :: [Card] -> Int
seed l = extractNumber (cardToString l) * 12345

getRandomIndex :: [Card] -> Int
getRandomIndex l = fst (randomR (0, length l - 1) (mkStdGen $ seed l))

-- Will throw an error if no cards of that suit is available. This renege is incomplete.
renege :: Suit -> [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> Card
renege _ hand _ Nothing = select $ filter (\x -> suit x == Club) hand where
    select :: [Card] -> Card
    select [] = findOtherSuit $ filter (\x -> suit x /= Heart && x /= Card Spade Queen) hand where
        findOtherSuit [] = hand !! getRandomIndex hand
        findOtherSuit l = l !! getRandomIndex l
    select l = l !! getRandomIndex l
renege leader hand _ _ = select $ filter (\x -> suit x == leader) hand where
    select :: [Card] -> Card
    -- select [] = if findTrickHeart currentTrick || findPreviousHeart previousTrick then head hand else findOtherSuit $ filter (\x -> suit x /= Heart) hand where
    --     findOtherSuit [] = hand !! getRandomIndex hand
    --     findOtherSuit l = l !! getRandomIndex l
    select [] = hand !! getRandomIndex hand
    select l = l !! getRandomIndex l

-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Maybe ([(Card, PlayerId)], String) -> Card
lead hand previous = select (findPreviousHeart previous) (find (== Card Club Two) hand) where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Bool -> Maybe Card -> Card
    select True Nothing = hand !! getRandomIndex hand
    select False Nothing = firstCard $ filter (\x -> suit x /= Heart) hand where
        firstCard :: [Card] -> Card
        firstCard [] = hand !! getRandomIndex hand
        firstCard l = l !! getRandomIndex l
    select _ card = fromJust card

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

cardToString :: [Card] -> String
cardToString = concatMap show

extractNumber :: String -> Int
extractNumber l = readNum $ filter ranks l where
    readNum :: String -> Int
    readNum [] = 0
    readNum r = read r :: Int
    ranks :: Char -> Bool
    ranks x
        | x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9' = True
        | otherwise = False

-- prev = Just ([(Card Diamond King, "1"), (Card Diamond Four, "2"), (Card Diamond Three, "3"), (Card Diamond Seven, "4")], "C2,CA,C4,C9,S9,S2,S10,SQ,S8,S3,SK,S7;1:0,2:0,3:13,4:0")
-- current = [(Card Diamond Two, "1"), (Card Diamond Ace, "2"), (Card Diamond Queen, "3"), (Card Diamond Nine, "4")]
-- hand = [Card Heart Six,Card Diamond Five,Card Heart King,Card Spade Ace,Card Club King,Card Diamond Jack,Card Club Three,Card Heart Four,Card Club Queen,Card Club Five,Card Heart Ten,Card Diamond Eight,Card Diamond Six]
-- shuffle :: [Card] -> [Card]
-- shuffle l = map snd $ sortOn fst $ zip i l where
--     i = take (length l) (randoms (mkStdGen (seed l)) :: [Double])

-- generateRestCards :: Maybe ([(Card, PlayerId)], String) -> [Card]
-- generateRestCards Nothing = []
-- generateRestCards (Just (prev, "")) = map fst prev
-- generateRestCards (Just (prev, memo)) = map fst prev ++ map (\ x -> read x :: Card) (wordsWhen (==',') (takeWhile (/= ';') memo))

-- getNumberOfOpponent :: Maybe ([(Card, PlayerId)], String) -> [Card] -> Int
-- getNumberOfOpponent p hand = (52 `div` ((length (generateRestCards p) - 4) `div` 4 + length hand)) - 1

-- opponentCards :: Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> [Card] -> [Card]
-- opponentCards prev current hand = shuffle (eliminate sortedDeck (generateRestCards prev ++ hand ++ map fst current))

-- distributeCard :: Maybe ([(Card, PlayerId)], String) -> PlayerId -> [Card] -> [Card] -> [(Card, PlayerId)] -> [(PlayerId, [Card])]
-- distributeCard Nothing _ _ _ _ = []
-- distributeCard p@(Just (prev, _)) i hand l c = zip currentIds (take (length currentIds) cards) ++ zip ids (drop (length currentIds) cards) where
--     ids = eliminate (delete i (map snd prev)) currentIds
--     currentIds = delete i (map snd c)
--     cardTemp = splitEvery (round (fromIntegral (length (eliminate l current)) / fromIntegral (getNumberOfOpponent p hand) :: Double) :: Int) l
--     cards :: [[Card]]
--     cards = sortBy (\ a b -> compare (length a) (length b)) temp where 
--         temp = if length cardTemp /= getNumberOfOpponent p hand then (head (last cardTemp) : head cardTemp) : delete (last cardTemp) (tail cardTemp) else cardTemp
--     current = map fst c

-- -- Source: https://stackoverflow.com/questions/8680888/subdividing-a-list-in-haskell
-- splitEvery :: Int -> [Card] -> [[Card]]
-- -- splitEvery _ [] = []
-- splitEvery n list = h : splitEvery n rest
--     where
--         (h,rest) = splitAt n list

-- Source: https://stackoverflow.com/questions/33017435/how-to-filter-a-list-by-another-list-in-haskell
-- eliminate :: Ord a => [a] -> [a] -> [a]
-- eliminate _ [] = []
-- eliminate [] _ = []
-- eliminate (x:xs) l
--     | x `notElem` l = x : eliminate xs l
--     | otherwise = eliminate xs l

-- determinization :: PlayerId -> [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, String)], String) -> [Card]
-- -- determinization i hand currentTrick prev = distributeCard prev i hand (opponentCards prev hand) currentTrick
-- determinization _ hand currentTrick prev = opponentCards prev currentTrick hand

-- ucb :: Floating a => a -> a -> a -> a
-- ucb w n np = (w / n) + (sqrt 2 * sqrt (log np / n))

-- maxUCB :: Tree MCTSNode -> Double
-- maxUCB tree = maximum ((\ x -> ucb (fromIntegral $ timesWon x) (fromIntegral $ timesSimulated x ) (fromIntegral $ parentTimesSimulated x) :: Double) <$> tree)

-- getNode :: Tree MCTSNode -> [MCTSNode]
-- getNode tree = foldr (\ x a -> if ucbScore x then x : a else a) [] tree where
--     maxS = maxUCB tree
--     ucbScore x = ucb (fromIntegral $ timesWon x) (fromIntegral $ timesSimulated x ) (fromIntegral $ parentTimesSimulated x) == maxS

data MCTSNode = MCTSNode {
    -- nodeId :: Int, 
    prevTrickMemo :: Maybe ([(Card, PlayerId)], String), 
    trickSoFar :: [(Card, PlayerId)], 
    hand:: [Card], 
    winnerId :: PlayerId, 
    timesWon :: Int, 
    timesSimulated :: Int, 
    parentTimesSimulated :: Int, 
    self :: Bool
} deriving (Show)

instance Eq MCTSNode where
    MCTSNode{prevTrickMemo = p, trickSoFar = c, hand = h, winnerId = w, self = s} == MCTSNode{prevTrickMemo = p', trickSoFar = c', hand = h', winnerId = w', self = s'} = (p == p') && (c == c') && (h == h') && (w == w') && (s == s')

instance Ord MCTSNode where
    compare MCTSNode{timesSimulated = ts} MCTSNode{timesSimulated = ts'} = compare ts ts'

-- expand :: PlayerId -> Tree MCTSNode -> MCTSNode -> [Card] -> (MCTSNode, [Card])
-- -- expand i tree (MCTSNode _ p c h w tw ts pts False) l = (MCTSNode nextId p (newCards ++ c) h w 0 1 ts True, eli) where
-- --     newCards = discard (4 - length c - 1) c
-- --     nextId = maximum (nodeId <$> tree) + 1
-- --     card n ct | not (null ct) = renege (suit $ fst $ last ct) (map snd eligiblePlayer !! n) ct p | otherwise = lead (map snd eligiblePlayer !! n) p
-- --     currentIds = map snd c
-- --     eligiblePlayer = filter (\ x -> fst x `notElem` currentIds) l
-- --     discard :: Int -> [(Card, PlayerId)] -> [(Card, PlayerId)]
-- --     discard (-1) _ = []
-- --     discard n ct = discard (n - 1) (tempCard ++ ct) ++ tempCard where 
-- --         tempCard = [(card n ct, fst (eligiblePlayer !! n))]
-- --     eli = map (\ (q,p) -> (q,eliminate p (map fst newCards))) l
-- expand _ _ (MCTSNode p c h w tw ts _ False) l
--     | getNumberOfOpponent p h == 3 && not (null c) = (MCTSNode p (newCards ++ c) h w 0 0 ts False, eli)
--     | getNumberOfOpponent p h == 1 && not (null c) = (MCTSNode p (discard ++ c) h w 0 0 ts False, eli)
--     | otherwise = (MCTSNode p (randomNumberOfNewCards ++ c) h w 0 0 ts True, eli')
--         where
--             randomNumberOfNewCards :: [(Card, PlayerId)]
--             randomNumberOfNewCards = newCardsAux (abs (getRandomIndex l) `mod` (getNumberOfOpponent p h - 1)) (delete (fst $ head discard) l) ++ discard
--             numberOfCardsOnTrick :: Int
--             numberOfCardsOnTrick = length c
--             -- nextId :: Int
--             -- nextId = maximum (nodeId <$> tree) + 1
--             newCards :: [(Card, PlayerId)]
--             newCards = newCardsAux (4 - numberOfCardsOnTrick) l
--             newCardsAux :: Int -> [Card] -> [(Card, PlayerId)]
--             newCardsAux 0 _ = []
--             newCardsAux n list = (list !! ((simRandom list (map fst c) tw ts + n) `mod` length list),"Opponent") : newCardsAux (n - 1) list
--             discard :: [(Card, PlayerId)]
--             discard | not (null c) = [(renege' (suit $ fst $ last c) l c p tw ts, "Opponent")] | otherwise = [(lead' l p c tw ts, "Opponent")]
--             eli :: [Card]
--             -- have to catch for 2 player version
--             eli | getNumberOfOpponent p h == 3 = eliminate l (map fst newCards) | otherwise = delete (fst $ head newCards) l
--                 | getNumberOfOpponent p h == 1 = delete (fst $ head discard) l
--             eli' :: [Card]
--             eli' | getNumberOfOpponent p h == 3 = eliminate l (map fst randomNumberOfNewCards) | otherwise = delete (fst $ head randomNumberOfNewCards) l
-- --                     | getNumberOfOpponent p h == 1 = delete (fst $ head discard) l
-- expand i _ (MCTSNode p c h w tw ts _ True) l = (MCTSNode p ((discard, i) : c) eli w 0 0 ts False, l)
--  where
--     -- nextId = maximum (nodeId <$> tree) + 1
--     discard | not (null c) = renege' (suit $ fst $ last c) h c p tw ts | otherwise = lead' h p c tw ts
--     -- need to modify the above function to make it random
--     eli = delete discard h

-- fullTrick :: (MCTSNode, [Card]) -> Int -> Bool
-- fullTrick (MCTSNode _ [_, _] _ _ _ _ _ _, _) 2 = True
-- fullTrick (MCTSNode _ [_, _, _, _] _ _ _ _ _ _, _) 4 = True
-- fullTrick _ _ = False

-- initialState :: (MCTSNode, [Card]) -> Bool -> PlayerId -> (MCTSNode, [Card])
-- initialState a False _ = a
-- initialState (MCTSNode p c h _ tw ts pts _, l) True selfId = (MCTSNode (Just (c, newMemo p)) [] h winId tw ts pts (winId == selfId), l) where
--     winId = wonPlayer (filterLeadingSuitCards (Just (c, "")))

-- retrieveNodeIfPresent :: (MCTSNode, [Card]) -> Bool -> Tree MCTSNode -> (MCTSNode, [Card])
-- retrieveNodeIfPresent (ini, l) True tree = (head $ filter (== ini) (flatten tree), l)
-- retrieveNodeIfPresent a False _ = a

-- Will throw an error if no cards of that suit is available. This renege is incomplete.
-- renegeAux :: Suit -> [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> [Card]
-- renegeAux _ hand _ Nothing = select $ filter (\x -> suit x == Club) hand where
--     select :: [Card] -> [Card]
--     select [] = findOtherSuit $ filter (\x -> suit x /= Heart && x /= Card Spade Queen) hand where
--         findOtherSuit [] = hand
--         findOtherSuit l = l
--     select l = l
-- renegeAux leader hand _ _ = select $ filter (\x -> suit x == leader) hand where
--     select :: [Card] -> [Card]
--     select [] = hand
--     select l = l

-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
-- leadAux :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> [Card]
-- leadAux hand previous _ = select (findPreviousHeart previous) (find (== Card Club Two) hand) where
--     -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
--     select :: Bool -> Maybe Card -> [Card]
--     select True Nothing = hand
--     select False Nothing = firstCard $ filter (\x -> suit x /= Heart) hand where
--         firstCard :: [Card] -> [Card]
--         firstCard [] = hand
--         firstCard l = l
--     select _ card = [fromJust card]

-- renege' :: Suit -> [Card] -> [(Card, PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> Int -> Int -> Card
-- renege' s hand currentTrick prev tw ts = validCards !! simRandom validCards (map fst currentTrick) tw ts where
--     validCards = renegeAux s hand currentTrick prev 

-- lead' :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> Int -> Int -> Card
-- lead' hand previous currentTrick tw ts = validCards !! simRandom validCards (map fst currentTrick) tw ts where
--     validCards = leadAux hand previous currentTrick

-- simRandom :: [Card] -> [Card] -> Int -> Int -> Int
-- simRandom l currentTrick tw ts = abs ((getRandomIndex currentTrick + 1) * getRandomIndex l * simFactor) `mod` length l where
--     simFactor = fst $ random generator
--     -- 2n+1 to explicitly make the input into odd number because mkStdGen doesn't work for even number
--     generator = mkStdGen ((2 * (tw + ts + 1234567) + 1 :: Int) ^ (12345 :: Int) :: Int)

-- selection :: Tree MCTSNode -> MCTSNode
-- selection tree = head $ getNode tree

-- refresh :: Bool -> (MCTSNode, [Card]) -> PlayerId -> PlayerId -> (MCTSNode, [Card])
-- refresh True (q, p) wp i = (MCTSNode (Just (trickSoFar q, newMemo (prevTrickMemo q))) [] (hand q) wp (timesWon q)(timesSimulated q) (parentTimesSimulated q) (wp == i), p)
-- refresh False a _ _ = a

-- simulation :: PlayerId -> Bool -> State (MCTSNode, [Card]) (PlayerId, Int)
-- simulation i True = do
--     (s, _) <- get
--     let
--         -- wp = wonPlayer (filterLeadingSuitCards (Just (trickSoFar s, "")))
--         -- finalState = fst $ refresh True (s, c) wp i
--         final = filter (\ x -> fst x == i) (newScore (prevTrickMemo s))
--     return $ head final
-- simulation i False = do 
--     (s, c) <- get
--     let
--         newState = expand i (Node s []) s c
--         wp = wonPlayer (filterLeadingSuitCards (Just (trickSoFar (fst newState), "")))
--         noOfPlayer = getNumberOfOpponent (prevTrickMemo (fst newState)) (hand (fst newState)) + 1
--         nextState = refresh (length (trickSoFar (fst newState)) == noOfPlayer) newState wp i
--         lastRound = null $ snd newState
--     put nextState
--     return ("", 0)
    -- simulation i lastRound

-- winLose :: (PlayerId, Int) -> Int -> Bool
-- -- winLose (_, score) initialScore = score <= initialScore + ((26 - initialScore) `div` 4)
-- winLose (pid, score) initialScore = score == 0 || score == initialScore

-- nodePresent :: (MCTSNode,[Card]) -> Tree MCTSNode -> Bool
-- nodePresent q tree = fst q `elem` tree

-- insertNode :: MCTSNode -> (MCTSNode, [Card]) -> Tree MCTSNode -> Bool -> Tree MCTSNode
-- insertNode _ _ a@(Node _ _) True = a
-- insertNode se ex (Node x xs) False 
--     | x == se = Node x (Node (fst ex) [] : xs)
--     | otherwise = Node x (map (\ y -> insertNode se ex y False) xs)

-- need to update the parent simulation time also
-- backPropagation :: (MCTSNode, [Card]) -> Bool -> Tree MCTSNode ->  Tree MCTSNode
-- backPropagation ex wl a@(Node (MCTSNode p c h w tw ts pts self) xs) = Node (newX wl present) (map (updateParentSimulation . backPropagation ex wl) xs) where
--     newX win True = MCTSNode p c h w (ntw win) (nts True) (pts + 1) self
--     -- newX win pre = MCTSNode p c h w (ntw win) (nts pre) (pts + 1) self
--     newX _ False = MCTSNode p c h w tw ts (pts + 1) self
--     nts True = ts + 1
--     nts False = ts
--     ntw True = tw + 1
--     ntw False = tw
--     present :: Bool
--     present = nodePresent ex a
--     updateParentSimulation :: Tree MCTSNode -> Tree MCTSNode
--     updateParentSimulation (Node (MCTSNode p' c' h' w' tw' ts' _ self') xs') = Node (MCTSNode p' c' h' w' tw' ts' (nts present) self') xs'


-- checkPrevRound :: [Card] -> Maybe ([(Card, PlayerId)], String) -> [Card]
-- checkPrevRound [] (Just (p, _)) = map fst p
-- checkPrevRound c _ = c

-- need to take care of prev also
-- evaluation :: State (Tree MCTSNode) Card
-- evaluation = do
--     tree <- get
--     let 
--         maximumNode = maximum (levels tree !! 1)
--         currentCards = map fst (trickSoFar maximumNode)
--         finalCards = checkPrevRound currentCards (prevTrickMemo maximumNode)
--     return $ head finalCards

-- monteCarloSearch :: PlayerId -> Int -> Int -> Bool -> State (Tree MCTSNode) Card
-- monteCarloSearch _ _ 0 _ = evaluation
-- monteCarloSearch _ _ _ True = evaluation
-- monteCarloSearch pid initialScore n _ = do
--     tree <- get
--     let 
--         se = selection tree
--         de = determinization pid (hand se) (trickSoFar se) (prevTrickMemo se)
--         ex = expand pid tree se de
--         ft = fullTrick ex (getNumberOfOpponent (prevTrickMemo se) (hand se) + 1)
--         ini = initialState ex ft pid
--         bo = nodePresent ini tree
--         ini' = retrieveNodeIfPresent ini bo tree
--         sim = evalState (simulation pid False) ini'
--         wl = winLose sim initialScore
--         temp = insertNode se ini' tree bo
--         newTree = backPropagation ini' wl temp
--     put newTree
--     monteCarloSearch pid initialScore (n - 1) (length (hand $ fst ini') < 2)

-- eligibleCards :: 
-- treeInitialization :: PlayerId -> Maybe ([(Card, PlayerId)], String) -> [(Card, PlayerId)] -> [Card] -> Tree MCTSNode
-- treeInitialization pid prev current@[_, _, _] hand = Node (MCTSNode prev current hand pid 0 noOfEligibleCards noOfEligibleCards True) children where
--     eligibleCards = renegeAux (suit $ fst $ last current) hand current prev
--     noOfEligibleCards = length eligibleCards
--     children = map (\ x -> Node (MCTSNode (Just ((x, pid) : current, newMemo prev)) [] (delete x hand) pid 0 1 noOfEligibleCards False) []) eligibleCards
-- treeInitialization pid prev current hand = Node (MCTSNode prev current hand pid 0 noOfEligibleCards noOfEligibleCards True) children where
--     eligibleCards :: [(Card, PlayerId)] -> [Card]
--     eligibleCards [] = leadAux hand prev current
--     eligibleCards c = renegeAux (suit $ fst $ last c) hand current prev
--     noOfEligibleCards = length $ eligibleCards current
--     children = map (\ x -> Node (MCTSNode prev ((x, pid) : current) (delete x hand) pid 0 1 noOfEligibleCards False) []) $ eligibleCards current

-- prevM = Just ([(Card Club Seven, "2"), (Card Club Two, "1")], "")
-- -- current' = [(Card Heart Five, "1"),(Card Spade Five, "4"),(Card Heart Ace, "2")]
-- current' = []
-- hand' = map (\ x -> read x :: Card) ["S9","SJ","SK","D8","D4","S10","SQ","C6","C5","CA","H10","H9","H5","HK","H7","D3","D10","DQ","DJ","DK","S3","S6","S4","SA","HQ"]
-- test' :: Tree MCTSNode
-- -- test = Node (MCTSNode prevM current' hand' "2" 2 4 4 True) [Node (MCTSNode prevM [(Card Diamond Five, "3"), (Card Diamond Two, "1"), (Card Diamond Ace, "2")] [Card Heart Six,Card Heart King,Card Spade Ace,Card Club King,Card Diamond Jack,Card Club Three,Card Heart Four,Card Club Queen] "2" 0 1 4 False) [], Node (MCTSNode prevM [(Card Diamond Jack, "3"), (Card Diamond Two, "1"), (Card Diamond Ace, "2")] [Card Heart Six,Card Diamond Five,Card Heart King,Card Spade Ace,Card Club King,Card Club Three,Card Heart Four,Card Club Queen] "2" 2 2 4 False) [Node (MCTSNode (Just ([(Card Diamond Eight, "Opponent"), (Card Diamond Jack, "3"), (Card Diamond Two, "1"), (Card Diamond Ace, "2")], "DK,D4,D3,D7,C2,CA,C4,C9,S9,S2,S10,SQ,S8,S3,SK,S7;1:0,2:0,3:13,4:0")) [] [Card Heart Six,Card Diamond Five,Card Heart King,Card Spade Ace,Card Club King,Card Club Three,Card Heart Four,Card Club Queen] "3" 1 1 2 True) []]]
-- -- test = Node (MCTSNode prevM current' hand' "2" 0 2 2 True) [Node (MCTSNode prevM ((Card Diamond Five, "3") : current') (delete (Card Diamond Five) hand') "2" 0 1 2 False) [], Node (MCTSNode prevM ((Card Diamond Jack, "3") : current') (delete (Card Diamond Jack) hand') "2" 0 1 2 False) []]
-- test' = Node (MCTSNode prevM current' hand' "2" 0 3 3 True) [Node (MCTSNode prevM ((Card Heart Six, "3") : current') (delete (Card Heart Six) hand') "2" 0 1 3 False) [], Node (MCTSNode prevM ((Card Heart King, "3") : current') (delete (Card Heart King) hand') "2" 0 1 3 False) [], Node (MCTSNode prevM ((Card Heart Four, "3") : current') (delete (Card Heart Four) hand') "2" 0 1 3 False) []]

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
