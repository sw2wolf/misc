-- ghc -O2 -threaded -o xq_uci xq_uci.hs
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Directory (findExecutable)
import Data.Array
import Data.Char (isDigit, isSpace, isLower, ord, digitToInt, toLower)
import Data.List (intercalate, isPrefixOf, isInfixOf, find, foldl')
import Data.Maybe (fromMaybe)
import System.IO
import System.Process
import System.Timeout (timeout)
import Control.Monad (when)
import Control.Exception (try, SomeException, IOException, finally)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- =====================================================
-- Piece constants
-- =====================================================
pieceEmpty, redKing, redAdvisor, redBishop, redKnight, redRook, redCannon, redPawn :: Int
pieceEmpty   = 0
redKing      = 1
redAdvisor   = 2
redBishop    = 3
redKnight    = 4
redRook      = 5
redCannon    = 6
redPawn      = 7

blackKing, blackAdvisor, blackBishop, blackKnight, blackRook, blackCannon, blackPawn :: Int
blackKing    = 8
blackAdvisor = 9
blackBishop  = 10
blackKnight  = 11
blackRook    = 12
blackCannon  = 13
blackPawn    = 14

-- =====================================================
-- Types
-- =====================================================
type Move = (Int, Int, Int, Int)   -- fromR, fromC, toR, toC
data Board = Board
  { cells    :: Array (Int, Int) Int
  , redTurn  :: Bool
  , captured :: [Int]
  }

data Engine = Engine
  { engIn  :: Handle
  , engOut :: Handle
  , engPh  :: ProcessHandle
  }

-- =====================================================
-- Piece names
-- =====================================================
pieceName :: Int -> String
pieceName p
  | p == pieceEmpty   = "  "
  | p == redKing      = "帅"
  | p == redAdvisor   = "仕"
  | p == redBishop    = "相"
  | p == redKnight    = "马"
  | p == redRook      = "车"
  | p == redCannon    = "炮"
  | p == redPawn      = "兵"
  | p == blackKing    = "将"
  | p == blackAdvisor = "士"
  | p == blackBishop  = "象"
  | p == blackKnight  = "马"
  | p == blackRook    = "车"
  | p == blackCannon  = "炮"
  | p == blackPawn    = "卒"
  | otherwise         = "??"

-- =====================================================
-- Predicates
-- =====================================================
isRed, isBlack :: Int -> Bool
isRed   p = p >= 1 && p <= 7
isBlack p = p >= 8 && p <= 14

inBoard :: Int -> Int -> Bool
inBoard r c = r >= 0 && r <= 9 && c >= 0 && c <= 8

sameSide :: Int -> Int -> Bool
sameSide a b = (isRed a && isRed b) || (isBlack a && isBlack b)

inPalace :: Bool -> Int -> Int -> Bool
inPalace red r c
  | red       = r >= 0 && r <= 2 && c >= 3 && c <= 5
  | otherwise = r >= 7 && r <= 9 && c >= 3 && c <= 5

-- =====================================================
-- Board accessors
-- =====================================================
pieceAt :: Board -> Int -> Int -> Int
pieceAt b r c = cells b ! (r, c)

setPiece :: Board -> Int -> Int -> Int -> Board
setPiece b r c p = b { cells = cells b // [((r, c), p)] }

newBoard :: Board
newBoard =
  let emptyA   = array ((0,0),(9,8)) [ ((r,c), pieceEmpty) | r <- [0..9], c <- [0..8] ]
      pieces =
        [ (9,0,blackRook),   (9,1,blackKnight), (9,2,blackBishop)
        , (9,3,blackAdvisor),(9,4,blackKing),   (9,5,blackAdvisor)
        , (9,6,blackBishop), (9,7,blackKnight), (9,8,blackRook)
        , (7,1,blackCannon), (7,7,blackCannon)
        , (6,0,blackPawn),   (6,2,blackPawn),   (6,4,blackPawn)
        , (6,6,blackPawn),   (6,8,blackPawn)
        , (3,0,redPawn),     (3,2,redPawn),     (3,4,redPawn)
        , (3,6,redPawn),     (3,8,redPawn)
        , (2,1,redCannon),   (2,7,redCannon)
        , (0,0,redRook),     (0,1,redKnight),   (0,2,redBishop)
        , (0,3,redAdvisor),  (0,4,redKing),     (0,5,redAdvisor)
        , (0,6,redBishop),   (0,7,redKnight),   (0,8,redRook)
        ]
      placed = foldl' (\a (r,c,p) -> a // [((r,c),p)]) emptyA pieces
  in Board placed True []

-- =====================================================
-- FEN (FIXED: produce [Char] not [String])
-- =====================================================
pieceChar :: Int -> Char
pieceChar p
  | p == blackRook    = 'r'
  | p == blackKnight  = 'n'
  | p == blackBishop  = 'b'
  | p == blackAdvisor = 'a'
  | p == blackKing    = 'k'
  | p == blackCannon  = 'c'
  | p == blackPawn    = 'p'
  | p == redRook      = 'R'
  | p == redKnight    = 'N'
  | p == redBishop    = 'B'
  | p == redAdvisor   = 'A'
  | p == redKing      = 'K'
  | p == redCannon    = 'C'
  | p == redPawn      = 'P'
  | otherwise         = '?'

toFen :: Board -> String
toFen b =
  let rowsStr = map (fenRow b) [9,8..0]
      body    = intercalate "/" rowsStr
      turn    = if redTurn b then " w" else " b"
  in body ++ turn ++ " - - 0 1"

fenRow :: Board -> Int -> String
fenRow b r = reverse (go 0 0 [])
  where
    go :: Int -> Int -> [Char] -> [Char]
    go 9 emptyAcc acc =
      if emptyAcc == 0 then acc else show emptyAcc ++ acc
    go c emptyAcc acc =
      let p = pieceAt b r c
      in if p == pieceEmpty
        then go (c+1) (emptyAcc+1) acc
        else
          let acc1 = if emptyAcc > 0 then show emptyAcc ++ acc else acc
              acc2 = pieceChar p : acc1
          in go (c+1) 0 acc2

-- =====================================================
-- Check detection
-- =====================================================
findKing :: Board -> Bool -> Maybe (Int, Int)
findKing b redSide = findPiece b (if redSide then redKing else blackKing) 0 0

findPiece :: Board -> Int -> Int -> Int -> Maybe (Int, Int)
findPiece _ _ 10 _ = Nothing
findPiece b t r 9  = findPiece b t (r+1) 0
findPiece b t r c
  | pieceAt b r c == t = Just (r, c)
  | otherwise          = findPiece b t r (c+1)

clearH :: Board -> Int -> Int -> Int -> Bool
clearH b r c1 c2 = all (\c -> pieceAt b r c == pieceEmpty) [lo..hi]
  where lo = min c1 c2 + 1; hi = max c1 c2 - 1

clearV :: Board -> Int -> Int -> Int -> Bool
clearV b c r1 r2 = all (\r -> pieceAt b r c == pieceEmpty) [lo..hi]
  where lo = min r1 r2 + 1; hi = max r1 r2 - 1

rookAttacksP :: Board -> Int -> Int -> Int -> Int -> Bool
rookAttacksP b r c kr kc =
  (r == kr && c /= kc && clearH b r c kc) ||
  (c == kc && r /= kr && clearV b c r kr)

knightAttacksP :: Board -> Int -> Int -> Int -> Int -> Bool
knightAttacksP b r c kr kc =
  let dr = abs (r - kr); dc = abs (c - kc)
  in if (dr == 2 && dc == 1) || (dr == 1 && dc == 2)
    then
      let (br, bc)
            | dr == 2 = ((r + kr) `div` 2, c)
            | dc == 2 = (r, (c + kc) `div` 2)
            | otherwise = (-1,-1)
      in inBoard br bc && pieceAt b br bc == pieceEmpty
    else False

cannonAttacksP :: Board -> Int -> Int -> Int -> Int -> Bool
cannonAttacksP b r c kr kc
  | r == kr && c /= kc =
    length [() | cc <- [min c kc + 1 .. max c kc - 1], pieceAt b r cc /= pieceEmpty] == 1
  | c == kc && r /= kr =
    length [() | rr <- [min r kr + 1 .. max r kr - 1], pieceAt b rr c /= pieceEmpty] == 1
  | otherwise = False

redPawnAttacksP :: Int -> Int -> Int -> Int -> Bool
redPawnAttacksP r c kr kc =
  (r + 1 == kr && c == kc) ||
  (r == kr && abs (c - kc) == 1 && r >= 5)

blackPawnAttacksP :: Int -> Int -> Int -> Int -> Bool
blackPawnAttacksP r c kr kc =
  (r - 1 == kr && c == kc) ||
  (r == kr && abs (c - kc) == 1 && r <= 4)

kingAttacksP :: Board -> Int -> Int -> Int -> Int -> Bool
kingAttacksP b r c kr kc = c == kc && clearV b c r kr

attacksP :: Board -> Int -> Int -> Int -> Int -> Int -> Bool
attacksP b r c p kr kc
  | p == redRook   || p == blackRook   = rookAttacksP b r c kr kc
  | p == redKnight || p == blackKnight = knightAttacksP b r c kr kc
  | p == redCannon || p == blackCannon = cannonAttacksP b r c kr kc
  | p == redPawn                       = redPawnAttacksP r c kr kc
  | p == blackPawn                     = blackPawnAttacksP r c kr kc
  | p == redKing   || p == blackKing   = kingAttacksP b r c kr kc
  | otherwise                          = False

isInCheck :: Board -> Bool -> Bool
isInCheck b redSide = case findKing b redSide of
  Nothing     -> False
  Just (kr,kc) -> any (\(r,c) ->
    let p = pieceAt b r c
        enemy = if redSide then isBlack p else isRed p
    in enemy && attacksP b r c p kr kc) [(r,c) | r <- [0..9], c <- [0..8]]

-- =====================================================
-- Move generation
-- =====================================================
addIfValid :: Board -> Int -> Int -> Int -> Int -> Int -> [Move] -> [Move]
addIfValid b piece fr fc tr tc moves
  | not (inBoard tr tc) = moves
  | sameSide piece (pieceAt b tr tc) = moves
  | otherwise =
    let test = setPiece (setPiece b tr tc piece) fr fc pieceEmpty
    in if isInCheck test (isRed piece)
      then moves
      else (fr, fc, tr, tc) : moves

kingMoves :: Board -> Int -> Int -> Int -> [Move]
kingMoves b p r c = foldl' step [] [(-1,0),(1,0),(0,-1),(0,1)]
  where
    red = isRed p
    step acc (dr, dc) =
      let tr = r + dr; tc = c + dc
      in if inPalace red tr tc then addIfValid b p r c tr tc acc else acc

advisorMoves :: Board -> Int -> Int -> Int -> [Move]
advisorMoves b p r c = foldl' step [] [(-1,-1),(-1,1),(1,-1),(1,1)]
  where
    red = isRed p
    step acc (dr, dc) =
      let tr = r + dr; tc = c + dc
      in if inPalace red tr tc then addIfValid b p r c tr tc acc else acc

bishopMoves :: Board -> Int -> Int -> Int -> [Move]
bishopMoves b p r c = foldl' step [] [(-2,-2),(-2,2),(2,-2),(2,2)]
  where
    red = isRed p
    step acc (dr, dc) =
      let br = r + dr `div` 2; bc = c + dc `div` 2
          tr = r + dr;        tc = c + dc
      in if inBoard br bc && pieceAt b br bc == pieceEmpty
        then let ok = if red then tr >= 0 && tr <= 4 else tr >= 5 && tr <= 9
             in if ok then addIfValid b p r c tr tc acc else acc
        else acc

knightMoves :: Board -> Int -> Int -> Int -> [Move]
knightMoves b p r c = foldl' step [] deltas
  where
    deltas = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
    step acc (dr, dc)
      | not (inBoard tr tc) = acc
      | pieceAt b br bc == pieceEmpty = addIfValid b p r c tr tc acc
      | otherwise = acc
      where
        tr = r + dr
        tc = c + dc
        (br, bc)
          | abs dr == 2 = ((r + tr) `div` 2, c)
          | abs dc == 2 = (r, (c + tc) `div` 2)
          | otherwise   = (-1,-1)

rookMoves :: Board -> Int -> Int -> Int -> [Move]
rookMoves b p r c =
  foldl' (\acc (dr,dc) -> slideRook b p r c dr dc acc) [] [(-1,0),(1,0),(0,-1),(0,1)]

slideRook :: Board -> Int -> Int -> Int -> Int -> Int -> [Move] -> [Move]
slideRook b p fr fc dr dc = go (fr + dr) (fc + dc)
  where
    go tr tc acc
      | not (inBoard tr tc)              = acc
      | pieceAt b tr tc == pieceEmpty    = go (tr+dr) (tc+dc) (addIfValid b p fr fc tr tc acc)
      | sameSide p (pieceAt b tr tc)     = acc
      | otherwise                        = addIfValid b p fr fc tr tc acc

cannonMoves :: Board -> Int -> Int -> Int -> [Move]
cannonMoves b p r c =
  foldl' (\acc (dr,dc) -> slideCannon b p r c dr dc acc) []
  [(-1,0),(1,0),(0,-1),(0,1)]

slideCannon :: Board -> Int -> Int -> Int -> Int -> Int -> [Move] -> [Move]
slideCannon b p fr fc dr dc acc0 =
  let (acc1, tr1, tc1) = cannonEmpties b p fr fc (fr+dr) (fc+dc) dr dc acc0
  in if not (inBoard tr1 tc1)
      then acc1
      else let (tr3, tc3) = cannonSkip b (tr1+dr) (tc1+dc) dr dc
           in if not (inBoard tr3 tc3)
            then acc1
            else if sameSide p (pieceAt b tr3 tc3)
              then acc1
              else addIfValid b p fr fc tr3 tc3 acc1

cannonEmpties :: Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Move] -> ([Move], Int, Int)
cannonEmpties b p fr fc tr tc dr dc acc
  | not (inBoard tr tc)           = (acc, tr, tc)
  | pieceAt b tr tc == pieceEmpty = cannonEmpties b p fr fc (tr+dr) (tc+dc) dr dc (addIfValid b p fr fc tr tc acc)
  | otherwise                     = (acc, tr, tc)

cannonSkip :: Board -> Int -> Int -> Int -> Int -> (Int, Int)
cannonSkip b tr tc dr dc
  | not (inBoard tr tc)           = (tr, tc)
  | pieceAt b tr tc == pieceEmpty = cannonSkip b (tr+dr) (tc+dc) dr dc
  | otherwise                     = (tr, tc)

pawnMoves :: Board -> Int -> Int -> Int -> [Move]
pawnMoves b p r c
  | isRed p =
    let a1 = addIfValid b p r c (r+1) c []
    in if r >= 5
      then let a2 = addIfValid b p r c r (c-1) a1
           in addIfValid b p r c r (c+1) a2
      else a1
  | otherwise =
    let a1 = addIfValid b p r c (r-1) c []
    in if r <= 4
      then let a2 = addIfValid b p r c r (c-1) a1
           in addIfValid b p r c r (c+1) a2
      else a1

generatePieceMoves :: Board -> Int -> Int -> [Move]
generatePieceMoves b r c =
  let p = pieceAt b r c
  in if p == pieceEmpty                          then []
    else if p == redKing    || p == blackKing    then kingMoves b p r c
    else if p == redAdvisor || p == blackAdvisor then advisorMoves b p r c
    else if p == redBishop  || p == blackBishop  then bishopMoves b p r c
    else if p == redKnight  || p == blackKnight  then knightMoves b p r c
    else if p == redRook    || p == blackRook    then rookMoves b p r c
    else if p == redCannon  || p == blackCannon  then cannonMoves b p r c
    else if p == redPawn    || p == blackPawn    then pawnMoves b p r c
    else []

generateAllMoves :: Board -> [Move]
generateAllMoves b =
  let turnRed = redTurn b
      isOwner p = if turnRed then isRed p else isBlack p
  in concatMap (\(r,c) ->
    let p = pieceAt b r c
    in if p /= pieceEmpty && isOwner p
      then generatePieceMoves b r c
      else []) [(r,c) | r <- [0..9], c <- [0..8]]

applyMove :: Board -> Move -> Board
applyMove b (fr, fc, tr, tc) =
  let p    = pieceAt b fr fc
      capP = pieceAt b tr tc
      b1   = setPiece b tr tc p
      b2   = setPiece b1 fr fc pieceEmpty
  in b2 { redTurn = not (redTurn b), captured = capP : captured b }

-- =====================================================
-- UCI move parsing (FIXED coordinate mapping)
-- =====================================================
uciMoveToMove :: Board -> String -> Maybe Move
uciMoveToMove b (fcChar:frChar:tcChar:trChar:_)
  | isLower fcChar && isDigit frChar && isLower tcChar && isDigit trChar =
    let fromC = ord fcChar - ord 'a'
        fromR = digitToInt frChar
        toC   = ord tcChar - ord 'a'
        toR   = digitToInt trChar
    in if inBoard fromR fromC && inBoard toR toC
      then findMove (generatePieceMoves b fromR fromC) toR toC
      else Nothing
uciMoveToMove _ _ = Nothing

findMove :: [Move] -> Int -> Int -> Maybe Move
findMove moves tr tc = find (\(_,_,r,c) -> r == tr && c == tc) moves

-- =====================================================
-- Engine I/O (System.Process)
-- =====================================================
sendCmd :: Handle -> String -> IO ()
sendCmd h cmd = hPutStrLn h cmd >> hFlush h

waitForToken :: Handle -> String -> Int -> IO Bool
waitForToken hout token timeoutUs = do
  r <- timeout timeoutUs go
  return (fromMaybe False r)
  where
    go = do
      r <- try (hGetLine hout) :: IO (Either IOException String)
      case r of
        Left _      -> return False
        Right line  -> if token `isInfixOf` line then return True else go

openEngine :: FilePath -> IO (Either String Engine)
openEngine pRaw = do
  -- resolve path: if no slash, lookup in PATH
  mbResolved <-
    if '/' `elem` pRaw
      then pure (Just pRaw)
      else findExecutable pRaw

  case mbResolved of
    Nothing ->
      pure $ Left ("无法在PATH中找到引擎: " ++ pRaw)

    Just enginePath -> do
      putStrLn $ "启动引擎: " ++ enginePath  -- debug print, you can remove later
      r <- try $ createProcess
             (proc enginePath [])
               { std_in  = CreatePipe
               , std_out = CreatePipe
               , std_err = NoStream
               } :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))

      case r of
        Left e -> pure $ Left ("创建进程失败: " ++ show e)
        Right (Just hin, Just hout, _, ph) -> do
          hSetBuffering hout LineBuffering
          hSetEncoding hout utf8
          hSetEncoding hin utf8

          -- handshake: send uci, wait uciok
          sendCmd hin "uci"
          okUci <- waitForToken hout "uciok" 60000000
          if not okUci
            then pure $ Left "握手失败: 未收到 uciok"
            else do
              sendCmd hin "isready"
              okReady <- waitForToken hout "readyok" 60000000
              if not okReady
                then pure $ Left "握手失败: 未收到 readyok"
                else pure $ Right (Engine hin hout ph)

        Right _ -> pure $ Left "管道创建失败: 缺少 stdin/stdout handle"

getBestMove :: Engine -> String -> Int -> IO (Either String String)
getBestMove eng posCmd movetimeMs = do
  sendCmd (engIn eng) "ucinewgame"
  sendCmd (engIn eng) posCmd
  sendCmd (engIn eng) ("go movetime " ++ show movetimeMs)
  r <- timeout 60000000 (bestMoveLoop (engOut eng))
  case r of
    Just x  -> return x
    Nothing -> return $ Left "timeout"
  where
    bestMoveLoop hout = do
      r <- try (hGetLine hout) :: IO (Either IOException String)
      case r of
        Left _     -> return $ Left "EOF or read error"
        Right line ->
          if "bestmove " `isPrefixOf` line
            then case words (drop 9 line) of
              (best:_) -> return $ Right best
              []       -> return $ Left "no bestmove arg"
            else bestMoveLoop hout

quitEngine :: Engine -> IO ()
quitEngine eng = do
  _ <- try (sendCmd (engIn eng) "quit") :: IO (Either SomeException ())
  _ <- try (terminateProcess (engPh eng)) :: IO (Either SomeException ())
  _ <- try (hClose (engIn eng))           :: IO (Either SomeException ())
  _ <- try (hClose (engOut eng))          :: IO (Either SomeException ())
  return ()

-- =====================================================
-- Rendering
-- =====================================================
colorRed, colorBlue, colorYellow, colorGreen :: String -> String
colorRed    s = "\ESC[1;31m" ++ s ++ "\ESC[0m"
colorBlue   s = "\ESC[1;34m" ++ s ++ "\ESC[0m"
colorYellow s = "\ESC[1;33m" ++ s ++ "\ESC[0m"
colorGreen  s = "\ESC[1;32m" ++ s ++ "\ESC[0m"

bgSelect, bgTarget :: String -> String
bgSelect s = "\ESC[44;1;37m" ++ s ++ "\ESC[0m"
bgTarget s = "\ESC[42;1;30m" ++ s ++ "\ESC[0m"

renderBoard :: Board -> Int -> Int -> [(Int,Int)] -> String -> String
renderBoard b selR selC targets engMoveStr =
  "\ESC[2J\ESC[H\n" ++
  concatMap (renderRow b selR selC targets) [9,8..0] ++ "        " ++ concatMap (\c -> show c ++ "    ") [0..8] ++ "\n\n" ++ "引擎: " ++ engMoveStr ++ "\n" ++ turnText ++ "\n\n"
  where
    turnText = if redTurn b
      then colorGreen "      ► 轮到: 红方 (帅)"
      else colorBlue  "      ► 轮到: 黑方 (将)"

renderRow :: Board -> Int -> Int -> [(Int,Int)] -> Int -> String
renderRow b selR selC targets r =
  "      " ++ show r ++ " " ++
  concatMap (\c ->
    renderCell b selR selC targets r c ++
    if c < 8
      then (if pieceAt b r c == pieceEmpty then "────" else "───")
      else "") [0..8] ++ "\n" ++ (if r > 0 then renderSeparator r else "")

renderSeparator :: Int -> String
renderSeparator 5 = "        │        ~~~~~             ~~~~~        │\n"
renderSeparator r = "        " ++ concatMap (\c -> sepCell c r) [0..8] ++ "\n"

sepCell :: Int -> Int -> String
sepCell c r
  | isDownRight = "│ ╲  "
  | isDownLeft  = "│ ╱  "
  | otherwise   = "│    "
  where
    isDownRight = (c == 3 && (r == 9 || r == 2)) || (c == 4 && (r == 8 || r == 1))
    isDownLeft  = (c == 3 && (r == 8 || r == 1)) || (c == 4 && (r == 9 || r == 2))

renderCell :: Board -> Int -> Int -> [(Int,Int)] -> Int -> Int -> String
renderCell b selR selC targets r c =
  let p = pieceAt b r c
      base = if p == pieceEmpty then "│" else pieceName p
      isSel = r == selR && c == selC
      isTgt = (r, c) `elem` targets
      withBg = if isSel then bgSelect base
        else if isTgt then bgTarget base
          else base
  in if p == pieceEmpty then withBg
    else if isRed p then colorRed withBg
    else colorBlue withBg

printBoard :: Board -> Int -> Int -> [(Int,Int)] -> String -> IO ()
printBoard b selR selC targets engMoveStr = do
  putStr $ renderBoard b selR selC targets engMoveStr
  hFlush stdout

printMoves :: Board -> [Move] -> IO ()
printMoves b moves = do
  n <- go 0 moves
  when (n `mod` 2 /= 0) $ putStrLn ""
  putStrLn ""
  where
    go i [] = return i
    go i ((_,_,tr,tc):rest) = do
      putStr $ "    " ++ show i ++ ": (" ++ show tr ++ "," ++ show tc ++ ")"
      let cap = pieceAt b tr tc
      when (cap /= pieceEmpty) $ putStr $ " [吃" ++ pieceName cap ++ "]"
      when (i `mod` 2 == 1) $ putStrLn ""
      go (i+1) rest

-- =====================================================
-- Input
-- =====================================================
data InputResult = InputLine String | InputEOF
readInput :: IO InputResult
readInput = do
  r <- try getLine :: IO (Either IOException String)
  return $ case r of
    Left _  -> InputEOF
    Right s -> InputLine (strip s)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

data CoordResult = CoordQuit | CoordOk Int Int
coordInput :: String -> IO CoordResult
coordInput prompt = do
  putStrLn $ "    <q:quit> " ++ prompt
  r <- readInput
  case r of
    InputEOF       -> return CoordQuit
    InputLine "q"  -> return CoordQuit
    InputLine (rr:cc:_) | isDigit rr && isDigit cc ->
      let rn = digitToInt rr; cn = digitToInt cc
      in if inBoard rn cn then return (CoordOk rn cn) else coordInput prompt
    InputLine _    -> coordInput prompt

data SelResult = SelQuit | SelOk Int Int
selectPiece :: Board -> IO SelResult
selectPiece b = do
  r <- coordInput "  选择棋子 (行列如32) "
  case r of
    CoordQuit     -> return SelQuit
    CoordOk fr fc -> do
      let p = pieceAt b fr fc
          rt = redTurn b
          isMyPiece = if rt then isRed p else isBlack p
      if p == pieceEmpty
        then do putStrLn "  ⚠ 此处无棋子！"; selectPiece b
        else if not isMyPiece
          then do
            putStrLn $ if rt then "  ⚠ 请选红方棋子！" else "  ⚠ 请选黑方棋子！"
            selectPiece b
          else case generatePieceMoves b fr fc of
            [] -> do putStrLn "  ⚠ 此棋子无合法走法！"; selectPiece b
            _  -> return $ SelOk fr fc

-- =====================================================
-- Turn handlers
-- =====================================================
data TurnResult = TurnQuit | TurnNext Board String | TurnError
playerTurn :: Board -> String -> IO TurnResult
playerTurn b engMoveStr = do
  r <- selectPiece b
  case r of
    SelQuit     -> return TurnQuit
    SelOk fr fc -> do
      let pieceMoves = generatePieceMoves b fr fc
          targets    = [ (tr, tc) | (_, _, tr, tc) <- pieceMoves ]
      printBoard b fr fc targets engMoveStr
      putStrLn "  可走位置:"
      printMoves b pieceMoves
      cr <- coordInput "  目标位置 (行列如64) "
      case cr of
        CoordQuit     -> return TurnQuit
        CoordOk tr tc -> case findMove pieceMoves tr tc of
          Nothing -> do
            putStrLn "  ✗ 非法走法！"
            playerTurn b engMoveStr
          Just mv -> do
            let cap  = pieceAt b tr tc
                newB = applyMove b mv
            when (cap /= pieceEmpty) $
              putStrLn $ "  ✓ 吃掉 " ++ pieceName cap ++ "!"
            return $ TurnNext newB engMoveStr

engineTurn :: Board -> Engine -> Int -> IO TurnResult
engineTurn b eng engTime = do
  putStrLn "  引擎思考中..."
  let posCmd = "position fen " ++ toFen b
  r <- getBestMove eng posCmd engTime
  case r of
    Left reason -> do
      putStrLn $ "  ✗ 引擎通信错误: " ++ reason
      return TurnError
    Right bestMoveStr -> case uciMoveToMove b bestMoveStr of
      Nothing -> do
        putStrLn $ "  ✗ 引擎返回非法走法: " ++ bestMoveStr
        return TurnError
      Just (fr, fc, tr, tc) -> do
        let newB    = applyMove b (fr, fc, tr, tc)
            moveStr = printf "%d,%d->%d,%d" fr fc tr tc
        return $ TurnNext newB moveStr

-- =====================================================
-- Game loop
-- =====================================================
gameLoop :: Board -> Engine -> Int -> Bool -> String -> IO ()
gameLoop b eng engTime playerIsRed engMoveStr = do
  printBoard b (-1) (-1) [] engMoveStr
  if null (generateAllMoves b)
  then do
    let rt = redTurn b
    if isInCheck b rt
    then if rt
      then do
        putStrLn $ colorBlue "═══════════════════════════════════════"
        putStrLn $ colorBlue "         黑方胜利！将死红帅！"
        putStrLn $ colorBlue "═══════════════════════════════════════"
      else do
        putStrLn $ colorRed "═══════════════════════════════════════"
        putStrLn $ colorRed "         红方胜利！将死黑将！"
        putStrLn $ colorRed "═══════════════════════════════════════"
    else do
      putStrLn $ colorYellow "═══════════════════════════════════════"
      putStrLn $ colorYellow "              和棋！无子可动"
      putStrLn $ colorYellow "═══════════════════════════════════════"
    putStrLn ""
    putStr "按 Enter 退出..."
    hFlush stdout
    _ <- readInput
    return ()
  else do
    let rt = redTurn b
    if rt == playerIsRed
    then do
      r <- playerTurn b engMoveStr
      case r of
        TurnQuit      -> return ()
        TurnNext nb _ -> gameLoop nb eng engTime playerIsRed engMoveStr
        TurnError     -> return ()
    else do
      r <- engineTurn b eng engTime
      case r of
        TurnQuit        -> return ()
        TurnNext nb nms -> gameLoop nb eng engTime playerIsRed nms
        TurnError       -> return ()

-- =====================================================
-- Main
-- =====================================================
main :: IO ()
main = do
  hSetEncoding  stdout utf8
  hSetEncoding  stdin  utf8
  hSetEncoding  stderr utf8
  hSetBuffering stdout LineBuffering
  let board = newBoard
  putStrLn ""
  putStr "  输入引擎路径 (default: pikafish): "
  hFlush stdout
  r1 <- readInput
  let enginePath = case r1 of
        --InputLine "" -> "/data/data/com.termux/files/home/bin/pikafish"
        InputLine "" -> "pikafish"
        InputLine p  -> p
        InputEOF     -> "pikafish"
  putStr "  引擎每步思考时间(ms) [默认9000]: "
  hFlush stdout
  r2 <- readInput
  let engTime = case r2 of
        InputLine s -> fromMaybe 9000 (readMaybe s :: Maybe Int)
        InputEOF    -> 9000
  putStr "  玩家执红先走? (y/n) [默认y]: "
  hFlush stdout
  r3 <- readInput
  let playerIsRed = case r3 of
        InputLine ""     -> True
        InputLine (c:_)  -> toLower c == 'y'
        InputEOF         -> True
  putStrLn ""
  putStrLn "  正在启动外部引擎 ..."
  result <- openEngine enginePath
  case result of
    Left err -> do
      putStrLn $ "  ✗ 无法启动外部引擎: " ++ err
      putStrLn "  请检查路径是否正确，按 Enter 退出..."
      hFlush stdout
      _ <- readInput
      return ()
    Right eng -> do
      putStrLn "  ✓ 已连接！"
      let ps = if playerIsRed then "红" else "黑"
          es = if playerIsRed then "黑" else "红"
      putStrLn $ "  玩家执" ++ ps ++ ", 外部引擎执" ++ es
      putStrLn ""
      gameLoop board eng engTime playerIsRed "" `finally` quitEngine eng
