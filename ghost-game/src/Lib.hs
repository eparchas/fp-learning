module Lib
    ( parseDictionary,
        calculateFirstLetters,
        toTrie
    ) where

import qualified Data.Trie as T
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BSU
import Data.List (sort, isPrefixOf, find)
import Debug.Trace ( trace )


calculateFirstLetters :: [String] -> [String]
calculateFirstLetters ws =
    let totalTrie      = toTrie ws
        playerAWinning = winningWords even totalTrie
        playerBWinning = winningWords odd totalTrie
        purePlays = play (PlayerA, PlayerB) "" playerAWinning playerBWinning
        playerAWins = snd <$> filter (\t -> fst t == PlayerA) purePlays
    in playerAWins

winningWords :: (Int -> Bool) -> T.Trie String -> T.Trie String
winningWords lengthFilter ws =
    -- Get words of matching length
    let lengthWords = filter (lengthFilter . length) (T.elems ws)
        -- Filter out words that start by other words in the dictionary (by checking the trie)
        pureWords = filter noFullWordPrefix lengthWords where
            noFullWordPrefix w = trace w (length (T.matches ws (BSU.pack w)) == 1)
    in toTrie pureWords

uniquePrefixes :: Int -> [String] -> [String]
uniquePrefixes l ws = uniq where
    longEnough = filter (\w -> length w >= l) ws
    prefixes = fmap (take l) longEnough
    uniq = S.toList $ S.fromList prefixes

data Tag = PlayerA | PlayerB deriving (Eq, Show)

play :: (Tag, Tag) -> String -> T.Trie String -> T.Trie String -> [(Tag, String)]
play (me, other) plays winning losing
    | T.member bsPlays losing = trace ("Won" ++ show (other, plays)) [(other, plays)]
    | T.member bsPlays winning = trace ("Won" ++ show (me, plays)) [(me, plays)]
    | otherwise = possiblePlays >>= (\p -> trace (show (me, p)) $ play (other, me) p losing winning) -- Swap the losing and winning arguments for the next player
    where prefixLength = length plays
          bsPlays = BSU.pack plays
          winningPlays = T.elems (T.submap bsPlays winning)
          losingPlays = T.elems (T.submap bsPlays losing)
          possiblePlays = if not (null winningPlays) then uniquePrefixes (prefixLength + 1) winningPlays
            else uniquePrefixes (prefixLength + 1) losingPlays

toTrie :: [String] -> T.Trie String
toTrie = foldl insert T.empty where
    insert t w = T.insert bs w t where
        bs = BSU.pack w

parseDictionary :: String -> IO [String]
parseDictionary _ = return ["cat", "calf", "dog", "bear"]