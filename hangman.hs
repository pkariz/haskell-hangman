import Data.List

sentence = "lala haha"

main = do
  putStrLn "Choose starting hp:"
  hp <- getLine
  start sentence (read hp ::Integer) []

start s hp used
  | hp == 0 = putStrLn ("gg dead, the solution was " ++ s)
  | (getUnique $ filter (\x -> x /= ' ') s)  \\ used == [] = putStrLn "gg!"
  | otherwise = do
    putStrLn output
    putStrLn "Enter a char:"
    c <- getLine
    putStrLn ""
    case c of
      [] -> start s hp used
      [x] -> 
        if x `elem` used
          then start s hp used
        else
          start s (if x `elem` s then hp else (hp-1)) (x:used)
      (x:xs) -> if (x:xs) == s then putStrLn "gg nice!" else putStrLn "idiot"
    where
      output = "solution so far: " ++ revealed ++ "\nused: " ++ usedRepr ++ ", hp: " ++ (show hp)
      revealed = getRevealed s used
      usedRepr = "[" ++ (intercalate ", " $ map (\x -> [x]) used) ++ "]"


getRevealed guessingStr usedChars = foldr reveal [] guessingStr
  where
    reveal c acc = (getValC c):acc
    getValC ' ' = ' '
    getValC c = if c `elem` usedChars then c else '_'

getUnique xs =
  getUnique' xs []
  where 
    getUnique' [] res = res
    getUnique' (x:xs) res
      | x `elem` xs = getUnique' xs res
      | otherwise = getUnique' xs (x:res)
