import System.IO
import Data.List

readInt :: String -> Int
readInt a = read a

buildFirstList :: [[String]] -> [Int]
buildFirstList li = map readInt $ map head li

buildSecondList:: [[String]] -> [Int]
buildSecondList li = map readInt $ map last li

readNumLine :: String -> [[String]]
readNumLine str = map words $ lines str

dist :: (Int, Int) -> Int
dist a = abs $ (fst a) - (snd a)

getDistance :: [Int] -> [Int] -> Int
getDistance x y = foldl (+) 0 $ map dist $ zip x y
	
main :: IO ()
main = do
	contents <- readFile "d1_input.txt"
	let n = readNumLine contents
	let a = sort $ buildFirstList n
	let b = sort $ buildSecondList n
	let res = getDistance a b
	print res 
