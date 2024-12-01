import System.IO
import Data.List

-- convert string to integer
readInt :: String -> Int
readInt a = read a

-- extract the first element of each sublist and convert to integer
buildFirstList :: [[String]] -> [Int]
buildFirstList li = map readInt $ map head li

-- extract the last element of each sublist and convert to integer
buildSecondList:: [[String]] -> [Int]
buildSecondList li = map readInt $ map last li

-- split multi-line string into a nested list of words
-- each line is split into a list of words and all lines are combined into list of lists
readNumLine :: String -> [[String]]
readNumLine str = map words $ lines str

-- absolute difference (distance) is calculated between two integers
dist :: (Int, Int) -> Int
dist a = abs $ (fst a) - (snd a)

-- calculates total distance between two lists of integers
-- each integer pair is passed to 'dist' and their results summed
getDistance :: [Int] -> [Int] -> Int
getDistance x y = foldl (+) 0 $ map dist $ zip x y

main :: IO ()
main = do
	-- read input file
	contents <- readFile "d1_input.txt"
	-- turns file into nested list of words
 	let n = readNumLine contents
  	-- builds and sorts list of integers from first element of each sublist
	let a = sort $ buildFirstList n
 	-- builds and sorts list of integers from last element of each sublist
	let b = sort $ buildSecondList n
 	-- calculates the distance between two sorted lists
	let res = getDistance a b
 	-- final answer
	print res 
