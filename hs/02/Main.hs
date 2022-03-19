data Direction = Forward | Down | Up

parseDirection :: String -> Direction
parseDirection str = case str of
    "forward" -> Forward
    "down" -> Down
    "up" -> Up

parseLine :: String -> (Direction, Int)
parseLine line = (parseDirection strDirection, read strAmount)
    where [strDirection, strAmount] = words line

parseInput :: FilePath -> IO [(Direction, Int)]
parseInput file = do
    contents <- readFile file
    let strLine = lines contents
    return (map parseLine strLine)

moveAim :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
moveAim (x, y, aim) (direction, amount) = case direction of
    Forward -> (x + amount, y + amount * aim, aim)
    Down -> (x, y, aim + amount)
    Up -> (x, y, aim - amount)

main :: IO ()
main = do
    directions <- parseInput "02/input.txt"

    let start = (0, 0, 0)
    let end@(x, y, aim) = foldl moveAim start directions
    print end

    -- part 1
    print (x * aim)
    -- part 2
    print (x * y)
