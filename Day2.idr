module Main

data Command
  = Forward Int
  | Down Int
  | Up Int
  | NoOp

calculatePosition : (Int, Int) -> Command -> (Int, Int)
calculatePosition orig@(horizontal, depth) command =
  case command of
       (Forward x) => (horizontal + x, depth)
       (Down x) => (horizontal, depth + x)
       (Up x) => (horizontal, depth - x)
       NoOp => orig


calculatePositionWithAim : (Int, Int, Int) -> Command -> (Int, Int, Int)
calculatePositionWithAim orig@(horizontal, depth, aim) command =
  case command of
       (Forward x) => (horizontal + x, depth + (aim * x), aim)
       (Down x) => (horizontal, depth, aim + x)
       (Up x) => (horizontal, depth, aim - x)
       NoOp => orig

part1 : List Command -> Int
part1 commands = let (horizontal, depth) = foldl calculatePosition (0, 0) commands in
                     horizontal * depth

part2 : List Command -> Int
part2 commands = let (horizontal, depth, aim) = foldl calculatePositionWithAim (0, 0, 0) commands in
                     horizontal * depth


main : IO ()
main = do file <- readFile "input/day2.txt"
          case file of
            Right content => putStr ("Part1: " ++ show (solve part1 content) ++ "\nPart2: " ++ show (solve part2 content) ++ "\n")
            Left err => printLn err
       where
         parseCommand : String -> Command
         parseCommand line =
            case span ((/=) ' ') line of
                 ("forward", val) => Forward (cast val)
                 ("down", val) => Down (cast val)
                 ("up", val) => Up (cast val)
                 _ => NoOp

         cleanInput : String -> List Command
         cleanInput input =
            map parseCommand (lines input)

         solve : (List Command -> Int) -> String -> Int
         solve solveFn input =
            solveFn (cleanInput input)
