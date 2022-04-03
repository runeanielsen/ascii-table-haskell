module Main where

import Data.List
import Text.Printf

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
  if i < 32
  then ' '
  else toEnum . fromEnum $ i

formatBlock :: Int -> String
formatBlock i =
  printf "%3d %4x %4o %2c" i i i (displayChar i)

tableRows :: [[Int]]
tableRows =
  do
    i <- [0 .. 31]
    return (map (\x -> i + (32 * x)) [0 .. 3])
