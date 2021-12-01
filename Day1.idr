module Main

countMeasurementsAbovePrevious : List Int -> Int
countMeasurementsAbovePrevious (x :: []) = 0
countMeasurementsAbovePrevious (x :: y :: xs) = if x < y then 1 + countMeasurementsAbovePrevious (y :: xs) else countMeasurementsAbovePrevious (y :: xs)

part1 : List Int -> Int
part1 input =
  countMeasurementsAbovePrevious input

slidingWindow : List Int -> List Int
slidingWindow (x :: y :: []) = []
slidingWindow (x :: y :: z :: xs) = x + y + z :: slidingWindow (y :: z :: xs)

part2 : List Int -> Int
part2 input =
        countMeasurementsAbovePrevious (slidingWindow input)


main : IO ()
main = do file <- readFile "input/day1.txt"
          case file of
            Right content => putStr ("Part1: " ++ show (solve part1 content) ++ "\nPart2: " ++ show (solve part2 content) ++ "\n")
            Left err => printLn err
       where
         cleanInput : String -> List Int
         cleanInput input =
            map cast (lines input)

         solve : (List Int -> Int) -> String -> Int
         solve solveFn input =
            solveFn (cleanInput input)
