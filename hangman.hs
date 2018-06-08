import Data.List


type Sentence = String
type Hp = Int
type UsedChars = [Char]

data State = State Sentence Hp UsedChars

data PlayState = Won | Lost | Play

main = do
  putStrLn "Choose starting hp:"
  line <- getLine
  game $ State sentence ((read line)::Hp) [] 
  where sentence = "lala haha" :: Sentence

game state@(State sentence hp used) = do
  case playState of
    Won -> putStrLn "gg!"
    Lost -> putStrLn ("lost, the solution was " ++ sentence)
    Play -> do
      putStrLn $ getStateRepr state
      line <- getLine
      game $ getNewState state line
  where playState = getPlayState state

getPlayState (State sentence hp used)
  | hp == 0 = Lost
  | (getUniqueIgnoreWs sentence) \\ used == [] = Won
  | otherwise = Play

getStateRepr (State sentence hp used) = stateRepr
  where
    stateRepr = revealed ++ "\nused: " ++ usedRepr ++ ", hp: " ++ (show hp)
    revealed = getRevealed sentence used
    usedRepr = "[" ++ (intercalate ", " $ map (\x -> [x]) used) ++ "]"

getNewState state@(State sentence hp used) line = case line of
  [] -> state
  [x] -> if x `elem` used then state else State sentence (hp-1) (x:used)
  (x:xs) -> if (x:xs) == sentence
    then State sentence hp $ getUniqueIgnoreWs sentence
    else State sentence 0 (x:used)

getRevealed guessingStr usedChars = foldr reveal [] guessingStr
  where
    reveal c acc = (getValC c):acc
    getValC ' ' = ' '
    getValC c = if c `elem` usedChars then c else '_'

getUniqueIgnoreWs = getUnique . filter (\x -> x /= ' ') 

getUnique xs =
  getUnique' xs []
  where 
    getUnique' [] res = res
    getUnique' (x:xs) res
      | x `elem` xs = getUnique' xs res
      | otherwise = getUnique' xs (x:res)
