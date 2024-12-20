module Day4 where

import Data.Hash.MD5 (Str (Str), md5s)
import Data.List (find, isPrefixOf)

md5 :: String -> String
md5 = md5s . Str

-- need to copy and paste input in ghci
part1 :: String -> Maybe String
part1 input = find (\x -> "00000" `isPrefixOf` md5 (input ++ x)) . map show $ [1 ..]

-- need to copy and paste input in ghci
part2 :: String -> Maybe String
part2 input = find (\x -> "000000" `isPrefixOf` md5 (input ++ x)) . map show $ [1 ..]
