type BinaryNumber = [Int]

parseBinary :: String -> BinaryNumber
parseBinary str = map parseDigit str
    where parseDigit d = if d == '1' then 1 else 0

parseReport :: FilePath -> IO [BinaryNumber]
parseReport file = do
    contents <- readFile file
    return $ map parseBinary $ lines contents

binaryToInt :: BinaryNumber -> Int
binaryToInt binaryNumber = sum $ zipWith (*) (exponentialFrom 1) (reverse binaryNumber)
    where exponentialFrom i = i : exponentialFrom (i * 2)

sumOnes :: [BinaryNumber] -> [Int]
sumOnes report = foldl sumDigits initialCount report
    where initialCount = repeat 0
          sumDigits = zipWith (+)

computeGammaRate :: [BinaryNumber] -> BinaryNumber
computeGammaRate report = map selectDigit oneCounts
    where selectDigit count = if count * 2 >= reportSize then 1 else 0
          reportSize = length report
          oneCounts = sumOnes report

computeEpsilonRate :: [BinaryNumber] -> BinaryNumber
computeEpsilonRate report = map (1-) (computeGammaRate report)

computeRating :: [BinaryNumber] -> ([BinaryNumber] -> BinaryNumber) -> BinaryNumber
computeRating report rateComputer = _computeRating report 0
    where _computeRating [number] _ = number
          _computeRating report   i = _computeRating (filter includeNumber report) (i + 1)
              where rateDigit = (rateComputer report) !! i
                    includeNumber binaryNumber = (binaryNumber !! i) == rateDigit

main = do
    report <- parseReport "03/input.txt"

    -- part 1
    let gammaRate = binaryToInt $ computeGammaRate report
    let epsilonRate = binaryToInt $ computeEpsilonRate report
    print $ gammaRate * epsilonRate -- 741950

    -- part 2
    let o2GeneratorRating = binaryToInt $ computeRating report computeGammaRate
    let cO2GeneratorRating = binaryToInt $ computeRating report computeEpsilonRate
    print $ o2GeneratorRating * cO2GeneratorRating -- 903810
