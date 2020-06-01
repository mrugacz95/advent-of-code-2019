import           Data.List.Split
import qualified Data.Map        as Map
import           System.IO

parse :: String -> Map.Map Integer Integer

parse input = Map.fromList $ zip [0..] $ map (read) (splitOn "," input)

data Computer = Computer {mem :: Map.Map Integer Integer, pointer :: Integer, relative_base :: Integer } deriving (Show)

read_mem :: Computer -> Integer -> Integer
read_mem computer pos = Map.findWithDefault 0 pos (mem computer)

shl :: Integer -> Integer -> Integer
shl num times
  | times == 0 = num
  | otherwise = shl (num * 10) (times - 1)

get_mode :: Computer -> Integer -> Integer
get_mode computer offset = read_mem computer (pointer computer) `div` (shl 10 offset)  `rem` 10

get_addr :: Computer -> Integer -> Integer
get_addr computer offset
  | mode == 0 = read_mem computer (pointer computer + offset)
  | mode == 1 = pointer computer + offset
  | mode == 2 = read_mem computer (pointer computer + offset) + (relative_base computer)
  where mode = get_mode computer offset

write_mem :: Computer -> Integer -> Integer -> Map.Map Integer Integer
write_mem c offset value =
  let addr = get_addr c offset
    in Map.insert addr value (mem c)

get_param :: Computer -> Integer -> Integer
get_param c offset =
  let addr = get_addr c offset
    in read_mem c addr

user_input :: IO Integer
user_input = do
          putStr "Input: "
          hFlush stdout
          number <- getLine
          return (read number :: Integer)

user_output :: Integer -> IO ()
user_output value = putStrLn $ "Output: " ++ show value

instr_cycle :: Computer -> IO Computer
instr_cycle comp = do
   let op = ((read_mem comp (pointer comp)) `rem` 100)
            in case op of
                   1 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { mem = write_mem comp 3 (a+b), pointer = (pointer comp) + 4}
                   2 -> let a = get_param comp 1
                            b = get_param comp 2
                        in instr_cycle comp { mem = write_mem comp 3 (a*b), pointer = (pointer comp) + 4}
                   3 -> do
                          value <- user_input
                          instr_cycle comp { mem = write_mem comp 1 value, pointer = (pointer comp) + 2}
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
    contents <- instr_cycle ( Computer{ mem = (parse contents), pointer = 0, relative_base = 0})
    putStrLn "Done"
    hClose handle
