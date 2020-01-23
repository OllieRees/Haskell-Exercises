import Data.List

-- First past the post

-- Count the number of votes a party gets
countParty :: Eq a => [a] -> a -> Int
countParty votes party = ( length . filter (==party) ) votes

-- Removes duplicate values from a list
removeDuplicate :: Eq a => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (vote:votes) = vote : filter (/= vote) ( removeDuplicate votes )

-- Sort the votes of each party from worst to best.
result :: Ord a => [a] -> [(Int, a)]
result votes = sort [ (countParty votes party, party) | party <- removeDuplicate votes  ]

fptpWinner :: Ord a => [a] -> a
fptpWinner votes  = ( snd . last . result) votes

-- Preferential Voting

-- Removes the empty ballots from the string
removeEmptyBallots :: Eq a => [[a]] -> [[a]]
removeEmptyBallots ballots = filter (/= []) ballots

-- Removes a given party
removeParty :: Eq a => [[a]] -> a -> [[a]]
removeParty ballots party = map ( filter (/= party) )ballots

-- Rank the parties from most successful to least
rank :: Ord a => [[a]] -> [a]
rank ballots = ( map snd . result . map head ) ballots

-- Get the winner of the vote from the ballots
avWinner :: Ord a => [[a]] -> a
avWinner ballots = case rank (removeEmptyBallots ballots) of
    [c]     -> c
    (c:cs)  -> avWinner ( removeParty ballots c)