import Data.List

-- Open is the place on board (1 - 9), Player is O or X
data Piece where
  Open :: Int -> Piece
  Player :: Char -> Piece
  deriving (Eq)

-- Define show for pieces, so it displays constructor arguments
instance Show Piece where
  show (Open n) = show n
  show (Player c) = [c]

-- Remove nth (index N - 1) item from a list
removeNth :: Int -> [a] -> ([a], [a])
removeNth index lst = (left, right)
  where
    (left, ys) = splitAt (index - 1) lst
    right = drop 1 ys

-- Put piece at position N
placePiece :: [a] -> a -> Int -> [a]
placePiece board piece index = xs ++ [piece] ++ ys
  where
    (xs, ys) = removeNth index board

pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _) = True
pieceIsOpen _ = False

-- Check if index is valid
openSpace :: [Piece] -> Int -> Bool
openSpace board index
  | length board < i = False
  | pieceIsOpen $ board !! i = True
  | otherwise = False
  where
    i = index - 1

-- Given a game board, get a valid position to place a piece
getPiecePosition :: [Piece] -> IO Int
getPiecePosition board = do
  putStrLn "Enter an open position (1 - 9):"
  input <- getChar
  if input `elem` ['1' .. '9'] && openSpace board (read [input])
    then return $ read [input]
    else do
      putStrLn "Enter an open position (1 - 9): "
      getPiecePosition board

showBoardLine :: [Piece] -> String
showBoardLine (a : b : c : xs) = show a ++ " | " ++ show b ++ " | " ++ show c
showBoardLine _ = error "List must contain at least three elements"

boardBorder :: String
boardBorder = "\n---------\n"

showBoard :: [Piece] -> String
showBoard board = "\n" ++ intercalate boardBorder [top, middle, bottom]
  where
    top = showBoardLine board
    middle = showBoardLine (drop 3 board)
    bottom = showBoardLine (drop 6 board)

swapPlayers :: Char -> Char
swapPlayers 'X' = 'O'
swapPlayers 'O' = 'X'
swapPlayers _ = error "swapPlayers only accepts X or O"

-- Given a board, player piece, and position on board check if the player won vertically
checkWinVert :: [Piece] -> Piece -> Int -> Bool
checkWinVert board player index = topPos == player && middlePos == player && bottomPos == player
  where
    topPos = board !! index
    middlePos = board !! (index + 3)
    bottomPos = board !! (index + 6)

playerWonVertically :: [Piece] -> Piece -> Bool
playerWonVertically board player = any (checkWinVert board player) [0, 1, 2]

checkWinHor :: [Piece] -> Piece -> Int -> Bool
checkWinHor board player index = firstPos == player && secondPos == player && thirdPos == player
  where
    firstPos = board !! index
    secondPos = board !! (index + 1)
    thirdPos = board !! (index + 2)

playerWonHorizontally :: [Piece] -> Piece -> Bool
playerWonHorizontally board player = any (checkWinHor board player) [0, 3, 6]

checkWinDiag :: [Piece] -> Piece -> Int -> Int -> Bool
checkWinDiag board player index step = firstPos == player && secondPos == player && thirdPos == player
  where
    firstPos = board !! index
    secondPos = board !! (index + step)
    thirdPos = board !! (index + 2 * step)

playerWonDiagnally :: [Piece] -> Piece -> Bool
playerWonDiagnally board player = wonFirstDiagonal || wonSecondDiagonal
  where
    wonFirstDiagonal = checkWinDiag board player 0 4
    wonSecondDiagonal = checkWinDiag board player 2 2

playerWon :: [Piece] -> Piece -> Bool
playerWon board player = playerWonDiagnally board player || playerWonHorizontally board player || playerWonVertically board player

tieGame :: [Piece] -> Bool
tieGame = not . any pieceIsOpen

-- Check if anyone won/tied
checkBoardState :: [Piece] -> Char -> IO ()
checkBoardState board playerChr
  | tieGame board = putStrLn "Its a tie!"
  | playerWon board (Player 'X') = putStrLn "Player X won!"
  | playerWon board (Player 'O') = putStrLn "Player O won!"
  | otherwise = runGame board (swapPlayers playerChr)

-- Main loop running the game
runGame :: [Piece] -> Char -> IO ()
runGame board playerChr = do
  putStrLn $ showBoard board
  rawChoice <- getPiecePosition board
  -- Create new board with Piece put by player
  let newBoard = placePiece board (Player playerChr) rawChoice
  checkBoardState newBoard playerChr

main :: IO ()
main = runGame board 'X'
  where
    board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]
