import qualified Data.Set as Set
import Data.List

type Board = [[Char]]

data Player = X | O deriving(Eq)

data BoardState = BoardState Board Player

data PlayState = XWon | OWon | End | Play

main = do
  putStrLn getGameRules
  game boardState
  where boardState = BoardState getInitialBoard X

getGameRules = "Positions:\n\
               \1 2 3\n\
               \4 5 6\n\
               \7 8 9\n\
               \Players: 'x' and 'o'"

getInitialBoard = [['_', '_', '_'],['_', '_', '_'],['_', '_', '_']]

removeEmptys = filter (\x -> x /= '_')

charToStr = (:[])

-- that way it's more readable to me :D
horizontals :: Board -> Board
horizontals = id

verticals (row1 : row2 : row3 : []) = zipWith3 (\x y z -> [x] ++ [y] ++ [z]) row1 row2 row3

diagonals ((x11:x12:x13:[]):(x21:x22:x23:[]):(x31:x32:x33:[]):[]) = [
  [x11] ++ [x22] ++ [x33],
  [x13] ++ [x22] ++ [x31]]
  
winScenarios board = horizontals board ++ verticals board ++ diagonals board

uniqueChars x = (length . Set.toList . Set.fromList) x == 1

getPlayState :: Board -> PlayState
getPlayState board = playState
  where 
    winningLine = removeEmptys $ map (\x -> x!!1) $ filter uniqueChars $ winScenarios board
    playState = case winningLine of
      [] -> if (length $ filter (\x -> x == '_') $ flattenList board) == 0
        then End else Play
      (x:xs) -> if x == 'x' then XWon else OWon

game boardState@(BoardState board player) = do
  case playState of
    XWon -> putStrLn "gg, X won"
    OWon -> putStrLn "gg, O won"
    End -> putStrLn "draw"
    Play -> do
      putStrLn $ getBoardRepr board
      putStrLn ((if player == X then "x" else "o") ++ " input position for the next play:")
      pos <- getLine
      game $ getNewState boardState ((read pos)::Int)
  where
    playState = getPlayState board

getBoardRepr board = intercalate "\n" $ map (intersperse ' ') board
    
getNewState boardState@(BoardState board player) pos
  | curPosVal == 'x' || curPosVal == 'o' = boardState
  | otherwise = newBoardState
  where 
    newBoardState = BoardState newBoard newPlayer
    newPlayer = if player == X then O else X
    val = if player == X then 'x' else 'o'
    newBoard = unflattenList 3 $ changeNth pos val flattened
    flattened = flattenList board
    curPosVal = flattened!!(pos-1)

changeNth n val xs = take (n-1) xs ++ [val] ++ drop n xs

flattenList :: [[Char]] -> [Char]
flattenList = foldl (\ys zs -> ys ++ foldl (\x y -> x ++ (charToStr y)) "" zs) []

unflattenList _ [] = []
unflattenList n xs = (take n xs) : unflattenList n (drop n xs)
