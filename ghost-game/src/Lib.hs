module Lib
  ( parseDictionary,
    calculateFirstLetters,
    toTrie,
    calculateWinningSequences,
  )
where

import Data.ByteString (hGetContents)
import qualified Data.ByteString.Char8 as BSU
import Data.List (find, intersperse, isPrefixOf, sort)
import qualified Data.Set as S
import qualified Data.Trie as T
import Debug.Trace (trace)
import System.IO (IOMode (ReadMode), hClose, openFile)

calculateFirstLetters :: [String] -> [String]
calculateFirstLetters = extractWinsFor PlayerA . calculateWinningSequences

data Tag = PlayerA | PlayerB deriving (Eq, Show)

extractWinsFor :: Tag -> [(Tag, String)] -> [String]
extractWinsFor tag wins =
  let orderFn     = case tag of
        PlayerA -> odd
        PlayerB -> even
      playerWins  = snd <$> filter (\t -> fst t == tag) wins
      playerPlays = fmap (fmap snd . filter (\(n, c) -> orderFn n) . zip [1 ..]) playerWins
   in fmap (intersperse ',') playerPlays

calculateWinningSequences :: [String] -> [(Tag, String)]
calculateWinningSequences ws =
  let totalTrie      = toTrie ws
      playerAWinning = winningWords even totalTrie
      playerBWinning = winningWords odd totalTrie
      winningPlays   = play (PlayerA, PlayerB) "" playerAWinning playerBWinning
   in winningPlays

winningWords :: (Int -> Bool) -> T.Trie String -> T.Trie String
winningWords lengthFilter ws =
  -- Get words of matching length
  let lengthWords = filter (lengthFilter . length) (T.elems ws)
      -- Filter out words that start by other words in the dictionary
      winningWords = filter noFullWordPrefix lengthWords
        where
          noFullWordPrefix w = length (T.matches ws (BSU.pack w)) == 1
   in toTrie winningWords

uniquePrefixes :: Int -> [String] -> [String]
uniquePrefixes l ws = uniq
  where
    longEnough = filter (\w -> length w >= l) ws
    prefixes = fmap (take l) longEnough
    uniq = S.toList $ S.fromList prefixes

play :: (Tag, Tag) -> String -> T.Trie String -> T.Trie String -> [(Tag, String)]
play (me, other) plays winning losing
  | T.member bsPlays losing = [(other, plays)]
  | T.member bsPlays winning = [(me, plays)]
  | otherwise = gameResult
  where
    prefixLength = length plays
    bsPlays = BSU.pack plays
    winningPlays = T.elems (T.submap bsPlays winning)
    losingPlays = T.elems (T.submap bsPlays losing)
    possibleWinningPlays = uniquePrefixes (prefixLength + 1) winningPlays
    gameResult =
      if not (null possibleWinningPlays)
        then possibleWinningPlays >>= (\p -> play (other, me) p losing winning) -- Swap the losing and winning arguments for the next player
        else [(other, plays)]

toTrie :: [String] -> T.Trie String
toTrie = foldl insert T.empty
  where
    insert t w = T.insert bs w t
      where
        bs = BSU.pack w

parseDictionary :: String -> IO [String]
parseDictionary filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let dict = words $ BSU.unpack contents
  hClose handle
  return dict