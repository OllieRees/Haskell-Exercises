import System.Random
import System.IO
import System.Environment
import Data.List

-- Gets n random values and presents in a list
finiteRandoms :: (RandomGen g, Random r) => Int -> g -> ([r], g)
finiteRandoms 0 gen = ([], gen) 						-- Base case 
finiteRandoms n gen = 
	let 	(value, newGen) 	= random gen 				-- Get random value of 'head' and new generator
		(restOfList, finalGen) 	= finiteRandoms (n - 1) newGen		-- Recursively call to get values of each 'head' with n representing the length of the list; use new generator
	in 	(value:restOfList, finalGen) 					-- return the list of randomly generated values and the final generator

-- Generate random string with random StdGen value
generateRandomString :: IO ()
generateRandomString = do
	gen		<-	getStdGen
	putStr		$ take 20 (randomRs ('a', 'z') gen)
	putStrLn	""

generateRandomStrings :: Int -> Int -> IO ()
generateRandomStrings strLen n = do
	if n > 0 then do
		gen		<-		newStdGen
		putStrLn	$ take strLen ( randomRs ('a', 'z') gen )
		generateRandomStrings strLen (n - 1)
	else do
		return ()

-- Infinite list of random values (use with take Int)
randoms' :: (RandomGen g, Random r) => g -> [r]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
	let 	(firstCoin, newGen)		= random gen
		(secondCoin, newGen') 		= random newGen		
		(thirdCoin, newGen'') 		= random newGen'	
	in (firstCoin, secondCoin, thirdCoin)

main = do
	args@(lenStr:numStr:_)	<-	getArgs
	if	(length args >= 2) then do
		let len 		= 	read lenStr
		let num			= 	read numStr
		generateRandomStrings len num
	else do
		putStrLn		"Not enough arguments given"
		return ()

