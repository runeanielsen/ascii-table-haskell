module Main where

import Data.List (intercalate)
import Text.Printf (printf)

main :: IO ()
main = putStrLn asciiTable

asciiTable :: String
asciiTable =
  intercalate "\n" $ headerRow : map bodyRow tableRows

headerRow :: String
headerRow =
  intercalate " | " . replicate 4 $ "Dec  Hex  Oct  C"

bodyRow :: [Int] -> String
bodyRow =
  intercalate " | " . map formatBlock

displayChar :: Int -> Char
displayChar i =
  if i < 32 || i == 127
  then ' '
  else toEnum i

formatBlock :: Int -> String
formatBlock i =
  printf "%3d %4x %4o %2c" i i i (displayChar i)

tableRows :: [[Int]]
tableRows =
  [take 4 [x, (x + 32)..] | x <- [0..31]]
