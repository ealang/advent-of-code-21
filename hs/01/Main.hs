parseInput :: FilePath -> IO [Int]
parseInput file = do
    contents <- readFile file
    let strLine = lines contents
    return (map read strLine)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs elems = zip elems (tail elems)

countIncreases :: [Int] -> Int
countIncreases depths = sum [ if b > a then 1 else 0 | (a, b) <- pairs depths ]

windowN :: Int -> [a] -> [[a]]
windowN n elems = if length elems >= n then (take n elems) : windowN n (tail elems)
                  else []

main :: IO ()
main = do
    depths <- parseInput "01/input.txt"

    -- part 1
    print (countIncreases depths)

    -- part 2
    let depths3 = map sum (windowN 3 depths)
    print (countIncreases depths3)
