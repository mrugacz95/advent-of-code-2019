import           Data.List.Split
import qualified Data.Map        as Map
import           System.IO
import Data.Char

parse :: String -> Map.Map Int Int
parse input = Map.fromList $ zip [0..] $ map (read) (splitOn "," input)

data Room = Room {items :: [String], name :: String}

data Computer = Computer {mem :: Map.Map Int Int, pointer :: Int, relative_base :: Int, buffor :: [Int] } deriving (Show)

read_mem :: Computer -> Int -> Int
read_mem computer pos = Map.findWithDefault 0 pos (mem computer)

shl :: Int -> Int -> Int
shl num times
  | times == 0 = num
  | otherwise = shl (num * 10) (times - 1)

get_mode :: Computer -> Int -> Int
get_mode computer offset = read_mem computer (pointer computer) `div` (shl 10 offset)  `rem` 10

get_addr :: Computer -> Int -> Int
get_addr computer offset
  | mode == 0 = read_mem computer (pointer computer + offset)
  | mode == 1 = pointer computer + offset
  | mode == 2 = read_mem computer (pointer computer + offset) + (relative_base computer)
  where mode = get_mode computer offset

write_mem :: Computer -> Int -> Int -> Map.Map Int Int
write_mem c offset value =
  let addr = get_addr c offset
    in Map.insert addr value (mem c)

get_param :: Computer -> Int -> Int
get_param c offset =
  let addr = get_addr c offset
    in read_mem c addr

user_input :: IO [Int]
user_input = do
          -- putStr "Input: "
          hFlush stdout
          command <- getLine
          return $ [ord c | c <- command] ++ [10]

user_output :: Int -> IO ()
user_output value = putChar (chr value)

instr_cycle :: Computer -> IO Computer
instr_cycle comp = do
   -- putStr $ show $ buffor comp
   let op = ((read_mem comp (pointer comp)) `rem` 100)
            in case op of
                   1 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { mem = write_mem comp 3 (a+b), pointer = (pointer comp) + 4}
                   2 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { mem = write_mem comp 3 (a*b), pointer = (pointer comp) + 4}
                   3 -> case buffor comp of
                            [] -> do
                                    value <- user_input
                                    instr_cycle comp { buffor = value }
                            (x:xs) -> instr_cycle comp { mem = write_mem comp 1 x, pointer = (pointer comp) + 2, buffor = xs }
                   4 -> let a = get_param comp 1
                        in do
                           user_output a
                           instr_cycle comp {  pointer = (pointer comp) + 2}
                   5 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { pointer = if a /= 0 then b else ((pointer comp) + 3) }
                   6 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { pointer = if a == 0 then b else ((pointer comp) + 3) }
                   7 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { mem = write_mem comp 3 (if a<b then 1 else 0), pointer = (pointer comp) + 4 }
                   8 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { mem = write_mem comp 3 (if a == b then 1 else 0), pointer = (pointer comp) + 4 }
                   9 -> let a = get_param comp 1
                        in instr_cycle comp { relative_base = (relative_base comp) + a, pointer = (pointer comp) + 2 }
                   99 -> return comp

main = do
    handle <- openFile "day_25.in" ReadMode
    contents <- hGetContents handle
    contents <- instr_cycle ( Computer{ mem = (parse contents), pointer = 0, relative_base = 0, buffor = []})
    putStrLn "Done"
    hClose handle
