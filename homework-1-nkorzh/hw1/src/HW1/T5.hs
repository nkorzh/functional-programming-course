module HW1.T5 where

import Data.List.NonEmpty (NonEmpty (..))

-- | addOrSkip :: token, accumulator -> result
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delim = foldr addOrSkip $ [] :| []
  where
    addOrSkip curToken (tokens :| parsedList)
      | curToken == delim = [] :| (tokens : parsedList)
      | otherwise = (curToken : tokens) :| parsedList

joinWith :: a -> NonEmpty [a] -> [a]
joinWith delim (x :| xs) = concat $ x : map (delim :) xs
