import System.IO
import qualified Data.Map as Map
import Data.List.Split

parse :: String -> Map.Map Integer Integer
parse input = Map.fromList $ zip [0..] $ map (read) (splitOn "," input)

data Computer = Computer {mem :: Map.Map Integer Integer, pointer :: Integer, relative_base :: Integer } deriving (Show)


read_mem :: Computer -> Integer -> Integer
read_mem computer pos = Map.findWithDefault 0 pos (mem computer)

get_mode :: Computer -> Integer -> Integer
get_mode computer offset = read_mem computer (pointer computer) `div` (10**offset) `rem` 10

get_addr :: Computer -> Integer -> Integer
get_addr computer offset
  | mode == 0 = read_mem computer (pointer computer + offset)
  | mode == 1 = pointer computer + offset
  | mode == 2 = read_mem computer (pointer computer + offset) + (relative_base computer)
  where mode = get_mode computer offset

write_mem :: Computer -> Integer -> Integer -> Map.Map Integer Integer
write_mem computer offset value =
  let addr = get_addr computer offset
    in Map.insert addr value (mem computer)

get_param :: Computer -> Integer -> Integer
get_param computer offset =
  let addr = get_addr computer offset
    in read_mem computer addr

user_input :: Computer -> IO Computer
user_input = do
          number <- getLine
          return (read number :: Integer)

user_output :: Computer -> Integer -> IO Computer
user_output computer value = do
  putStr $ show value
  return computer

instr_cycle :: Computer -> IO Computer
instr_cycle computer =
  let op = read_mem (pointer computer) `rem` 100
  in cycle (case op of
      1 -> let a = get_param computer 1
               b = get_param computer 2
           in computer { mem = write_mem computer 3 (a+b), pointer = (pointer computer) + 4}
      2 -> let a = get_param computer 1
               b = get_param computer 2
           in computer { mem = write_mem computer 3 (a*b), pointer = (pointer computer) + 4}
      3 ->  let a = get_param computer 1
            in do
              value <- user_input
              return computer { mem = write_mem computer a user_input, pointer = (pointer computer) + 2}
      4 -> let a = get_param computer 1
           in user_output computer a
      5 -> let a = get_param computer 1
               b = get_param computer 2
           in computer { pointer = if a /= 0 then b else ((pointer computer) + 3) }
      6 -> let a = get_param computer 1
               b = get_param computer 2
           in computer { pointer = if a == 0 then b else ((pointer computer) + 3) }
      7 -> let a = get_param computer 1
               b = get_param computer 2
           in computer { mem = write_mem computer 3 (if a<b then 1 else 0 user_input), pointer = (pointer computer) + 4 }
      8 -> let a = get_param computer 1
               b = get_param computer 2
           in computer { mem = write_mem computer 3 (if a == b then 1 else 0 user_input), pointer = (pointer computer) + 4 }
      9 -> let a = get_param computer 1
           in computer { relative_base = (relative_base computer) + a, pointer = (pointer computer) + 2 }
      99 -> return computer)


main = do
    handle <- openFile "day_25.in" ReadMode
    contents <- hGetContents handle
    putStr $ show (get_mode 1234 1)
    putStr $ show (parse contents)
    hClose handle
