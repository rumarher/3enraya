import Data.Maybe

data Player = X | O | E deriving(Eq)
data Position = Position Int Int deriving(Show)

instance Show Player where
   show X = show 'X'
   show O = show 'O'
   show E = show ' ' -- Empty

type Board = [[Player]]

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

obtDiagonal :: [[a]] -> [a]
obtDiagonal [[]]     = []
obtDiagonal (xs:[])  = [head xs]
obtDiagonal (x:xs)   = head x : obtDiagonal (map tail xs)

replaceElement :: Int -> a -> [a] -> [a]
replaceElement 0 e (x:xs) = e:xs
replaceElement n e (x:xs) = x : replaceElement (n - 1) e xs

allCombinations m = m ++ transpose m ++ [obtDiagonal m] ++ [obtDiagonal (transpose m)]

gameFinished :: Board -> Bool
gameFinished m = not $ elem True $ map (elem E) m

updateBoard :: Board -> Position -> Player -> Board
updateBoard g (Position i j) player = replaceElement i newRow g
   where newRow = replaceElement j player (g !! i)

gameWon :: Board -> Maybe Bool
gameWon m
   | 1 < abs (balance m) = Nothing
   | elem [X,X,X] (allCombinations m) && elem [O,O,O] (allCombinations m)  = Nothing
   | elem [O,O,O] (allCombinations m) || elem [X,X,X] (allCombinations m)  = Just True
   | elem True (map (elem E) (allCombinations m)) = Nothing
   | otherwise = Just False
   where
      balance = foldl (+) 0 . map ((foldl (+) 0).(map rate))   -- Point free style
         where rate p
                  | p == X = 1
                  | p == O = (-1)
                  | p == E = 0

emptyBoard = [[E,E,E],[E,E,E],[E,E,E]]

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X
otherPlayer E = E

playingWith :: Board -> Player -> IO ()
playingWith board player = do
   putStrLn (unlines (map show board))
   if gameFinished board then do
      putStrLn ("Ganaron: " ++ show (otherPlayer player : []))
   else do
      putStrLn ("Fila: ")
      i <- getLine
      putStrLn ("Columna")
      j <- getLine
      let pos = Position ((read i :: Int) - 1) ((read j :: Int) - 1)
      playingWith (updateBoard board pos player) (otherPlayer player)

main = do
   playingWith emptyBoard X

g1 = [[X,X,X],[E,O,O],[E,E,E]] -- Just True
g2 = [[X,O,X],[X,O,X],[O,X,O]] -- Just False
g3 = [[X,X,O],[X,X,O],[X,O,X]] -- Nothing
