import std/[syncio, os, osproc, strutils, locks, options]
import streams

type
  Result*[T, E] = object
    case isOk*: bool
    of true:
      v*: T
    of false:
      e*: E

proc Ok*[T, E](v: T): Result[T, E] =
  Result[T, E](isOk: true, v: v)

proc Err*[T, E](e: E): Result[T, E] =
  Result[T, E](isOk: false, e: e)

proc get*[T, E](r: Result[T, E]): T =
  if not r.isOk:
    raise newException(ValueError, "cannot get value of err Result")
  r.v

proc error*[T, E](r: Result[T, E]): E =
  if r.isOk:
    raise newException(ValueError, "cannot get error of ok Result")
  r.e

# ------------------------------
# Piece constants & display strings
# ------------------------------
const
  EMPTY*        = 0'u8
  R_KING*       = 1'u8
  R_ADVISOR*    = 2'u8
  R_BISHOP*     = 3'u8
  R_KNIGHT*     = 4'u8
  R_ROOK*       = 5'u8
  R_CANNON*     = 6'u8
  R_PAWN*       = 7'u8

  B_KING*       = 8'u8
  B_ADVISOR*    = 9'u8
  B_BISHOP*     = 10'u8
  B_KNIGHT*     = 11'u8
  B_ROOK*       = 12'u8
  B_CANNON*     = 13'u8
  B_PAWN*       = 14'u8

  PIECE_NAMES*: seq[string] = @[
    "  ", "帅", "仕", "相", "马", "车", "炮", "兵",
    "将", "士", "象", "马", "车", "炮", "卒"
  ]

  SPACE*        = " "
  H_LINE*       = "─"
  V_LINE*       = "│"
  SLASH*        = "╱"
  B_SLASH*      = "╲"

var
  #ENGINE_MOVE_LOCK*: Lock
  ENGINE_MOVE*: string

# ------------------------------
# Terminal color helpers
# ------------------------------
proc red(s: string): string    = "\x1b[1;31m" & s & "\x1b[0m"
proc blue(s: string): string   = "\x1b[1;34m" & s & "\x1b[0m"
proc yellow(s: string): string = "\x1b[1;33m" & s & "\x1b[0m"
proc green(s: string): string  = "\x1b[1;32m" & s & "\x1b[0m"
proc bgSelect(s: string): string = "\x1b[44;1;37m" & s & "\x1b[0m"
proc bgTarget(s: string): string = "\x1b[42;1;30m" & s & "\x1b[0m"

# ------------------------------
# Piece side utilities
# ------------------------------
proc isRed(p: uint8): bool    = p in 1'u8..7'u8
proc isBlack(p: uint8): bool  = p in 8'u8..14'u8

proc sameSide(a, b: uint8): bool =
  if a == 0'u8 or b == 0'u8:
    true
  else:
    (isRed(a) and isRed(b)) or (isBlack(a) and isBlack(b))

proc inBoard(r, c: int): bool = r >= 0 and r <= 9 and c >= 0 and c <= 8

# ------------------------------
# Core types
# ------------------------------
type
  Move* = object
    fromR, fromC, toR, toC: int
    score: int

  Board* = object
    board: array[10, array[9, uint8]]
    redTurn: bool
    history: seq[Move]
    captured: seq[uint8]

proc newMove(fr, fc, tr, tc: int): Move =
  Move(fromR: fr, fromC: fc, toR: tr, toC: tc, score: 0)

proc newBoard(): Board =
  var b: array[10, array[9, uint8]]
  b[0] = [B_ROOK, B_KNIGHT, B_BISHOP, B_ADVISOR, B_KING, B_ADVISOR, B_BISHOP, B_KNIGHT, B_ROOK]
  b[2] = [0'u8, B_CANNON, 0'u8, 0'u8, 0'u8, 0'u8, 0'u8, B_CANNON, 0'u8]
  b[3] = [B_PAWN, 0'u8, B_PAWN, 0'u8, B_PAWN, 0'u8, B_PAWN, 0'u8, B_PAWN]

  b[6] = [R_PAWN, 0'u8, R_PAWN, 0'u8, R_PAWN, 0'u8, R_PAWN, 0'u8, R_PAWN]
  b[7] = [0'u8, R_CANNON, 0'u8, 0'u8, 0'u8, 0'u8, 0'u8, R_CANNON, 0'u8]
  b[9] = [R_ROOK, R_KNIGHT, R_BISHOP, R_ADVISOR, R_KING, R_ADVISOR, R_BISHOP, R_KNIGHT, R_ROOK]

  result = Board(
    board: b,
    redTurn: true,
    history: newSeq[Move](),
    captured: newSeq[uint8]()
  )

# ------------------------------
# FEN serialization
# ------------------------------
proc toFen(b: Board): string =
  var fen = "" #newString()
  for r in 0..9:
    var emptyCount = 0
    for c in 0..8:
      let pc = b.board[r][c]
      if pc == 0'u8:
        inc emptyCount
      else:
        if emptyCount > 0:
          fen.add($emptyCount)
          emptyCount = 0

        let ch = case pc
          of B_ROOK:     'r'
          of B_KNIGHT:   'n'
          of B_BISHOP:   'b'
          of B_ADVISOR:  'a'
          of B_KING:     'k'
          of B_CANNON:   'c'
          of B_PAWN:     'p'
          of R_ROOK:     'R'
          of R_KNIGHT:   'N'
          of R_BISHOP:   'B'
          of R_ADVISOR:  'A'
          of R_KING:     'K'
          of R_CANNON:   'C'
          of R_PAWN:     'P'
          else:          '?'
        fen.add(ch)

    if emptyCount > 0:
      fen.add($emptyCount)
    if r < 9:
      fen.add('/')

  if b.redTurn:
    fen.add(" w - - 0 1")
  else:
    fen.add(" b - - 0 1")
  result = fen

proc containsTuple(s: seq[(int, int)], r, c: int): bool =
  for (sr, sc) in s:
    if sr == r and sc == c:
      return true
  false

# ------------------------------
# King & check detection
# ------------------------------
proc findKing(b: Board, red: bool): (int, int) =
  let target = if red: R_KING else: B_KING
  for r in 0..9:
    for c in 0..8:
      if b.board[r][c] == target:
        return (r, c)
  (-1, -1)

proc isInCheck(b: Board, red: bool): bool =
  let (kr, kc) = b.findKing(red)
  if kr < 0:
    return false

  for r in 0..9:
    for c in 0..8:
      let p = b.board[r][c]
      if p == 0'u8:
        continue
      if red and isRed(p):
        continue
      if not red and isBlack(p):
        continue

      case p
      of R_ROOK, B_ROOK:
        # same row
        if r == kr and c != kc:
          let step = if c < kc: 1 else: -1
          var cc = c + step
          var blocked = false
          while cc != kc:
            if b.board[r][cc] != 0'u8:
              blocked = true
              break
            cc += step
          if not blocked:
            return true
        # same column
        if c == kc and r != kr:
          let step = if r < kr: 1 else: -1
          var rr = r + step
          var blocked = false
          while rr != kr:
            if b.board[rr][c] != 0'u8:
              blocked = true
              break
            rr += step
          if not blocked:
            return true

      of R_KNIGHT, B_KNIGHT:
        let dr = abs(r - kr)
        let dc = abs(c - kc)
        if (dr == 2 and dc == 1) or (dr == 1 and dc == 2):
          let (br, bc) = if dr == 2: ((r + kr) div 2, c) else: (r, (c + kc) div 2)
          if inBoard(br, bc) and b.board[br][bc] == 0'u8:
            return true

      of R_CANNON, B_CANNON:
        # same row
        if r == kr and c != kc:
          let step = if c < kc: 1 else: -1
          var cnt = 0
          var cc = c + step
          while cc != kc:
            if b.board[r][cc] != 0'u8:
              inc cnt
            cc += step
          if cnt == 1:
            return true
        # same column
        if c == kc and r != kr:
          let step = if r < kr: 1 else: -1
          var cnt = 0
          var rr = r + step
          while rr != kr:
            if b.board[rr][c] != 0'u8:
              inc cnt
            rr += step
          if cnt == 1:
            return true

      of R_PAWN:
        if r - 1 == kr and c == kc:
          return true
        if r == kr and abs(c - kc) == 1 and r <= 5:
          return true

      of B_PAWN:
        if r + 1 == kr and c == kc:
          return true
        if r == kr and abs(c - kc) == 1 and r >= 4:
          return true

      of R_KING, B_KING:
        if c == kc:
          var blocked = false
          for rr in min(r, kr) + 1 .. max(r, kr) - 1:
            if b.board[rr][c] != 0'u8:
              blocked = true
              break
          if not blocked:
            return true
      else:
        discard
  false

# ------------------------------
# Move generation
# ------------------------------
proc generatePieceMoves(b: Board, r, c: int): seq[Move] =
  var moves = newSeq[Move]()
  let p = b.board[r][c]
  if p == EMPTY:
    return

  proc addIfValid(base: Board, piece: uint8, tr, tc: int) =
    if not inBoard(tr, tc):
      return
    let tp = base.board[tr][tc]
    if tp != 0'u8 and sameSide(piece, tp):
      return

    var ns = base
    ns.board[tr][tc] = piece
    ns.board[r][c] = EMPTY
    if not ns.isInCheck(isRed(piece)):
      moves.add(newMove(r, c, tr, tc))

  case p
  of R_KING, B_KING:
    for dr in [-1, 0, 1]:
      for dc in [-1, 0, 1]:
        if abs(dr) + abs(dc) != 1:
          continue
        let tr = r + dr
        let tc = c + dc
        if isRed(p):
          if tr in 7..9 and tc in 3..5:
            addIfValid(b, p, tr, tc)
        else:
          if tr in 0..2 and tc in 3..5:
            addIfValid(b, p, tr, tc)

  of R_ADVISOR, B_ADVISOR:
    for dr in [-1, 0, 1]:
      for dc in [-1, 0, 1]:
        if abs(dr) != 1 or abs(dc) != 1:
          continue
        let tr = r + dr
        let tc = c + dc
        if isRed(p):
          if tr in 7..9 and tc in 3..5:
            addIfValid(b, p, tr, tc)
        else:
          if tr in 0..2 and tc in 3..5:
            addIfValid(b, p, tr, tc)

  of R_BISHOP, B_BISHOP:
    for dr in [-2, 2]:
      for dc in [-2, 2]:
        if abs(dr) != 2 or abs(dc) != 2:
          continue
        let br = r + dr div 2
        let bc = c + dc div 2
        if not inBoard(br, bc) or b.board[br][bc] != 0'u8:
          continue
        let tr = r + dr
        let tc = c + dc
        if isRed(p):
          if tr in 5..9:
            addIfValid(b, p, tr, tc)
        else:
          if tr in 0..4:
            addIfValid(b, p, tr, tc)

  of R_KNIGHT, B_KNIGHT:
    let offsets = @[(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
    for (dr, dc) in offsets:
      let tr = r + dr
      let tc = c + dc
      if not inBoard(tr, tc):
        continue
      let (br, bc) = if abs(dr) == 2: (r + dr div 2, c) else: (r, c + dc div 2)
      if inBoard(br, bc) and b.board[br][bc] == 0'u8:
        addIfValid(b, p, tr, tc)

  of R_ROOK, B_ROOK:
    for dr in [-1, 0, 1]:
      for dc in [-1, 0, 1]:
        if abs(dr) + abs(dc) != 1:
          continue
        var tr = r + dr
        var tc = c + dc
        while inBoard(tr, tc):
          let tp = b.board[tr][tc]
          if tp == 0'u8:
            addIfValid(b, p, tr, tc)
          else:
            if not sameSide(p, tp):
              addIfValid(b, p, tr, tc)
            break
          tr += dr
          tc += dc

  of R_CANNON, B_CANNON:
    for dr in [-1, 0, 1]:
      for dc in [-1, 0, 1]:
        if abs(dr) + abs(dc) != 1:
          continue
        var tr = r + dr
        var tc = c + dc
        # slide empty squares
        while inBoard(tr, tc) and b.board[tr][tc] == 0'u8:
          addIfValid(b, p, tr, tc)
          tr += dr
          tc += dc
        # jump over one piece to capture
        tr += dr
        tc += dc
        while inBoard(tr, tc):
          let tp = b.board[tr][tc]
          if tp != 0'u8:
            if not sameSide(p, tp):
              addIfValid(b, p, tr, tc)
            break
          tr += dr
          tc += dc

  of R_PAWN:
    addIfValid(b, p, r - 1, c)
    if r <= 5:
      addIfValid(b, p, r, c - 1)
      addIfValid(b, p, r, c + 1)

  of B_PAWN:
    addIfValid(b, p, r + 1, c)
    if r >= 4:
      addIfValid(b, p, r, c - 1)
      addIfValid(b, p, r, c + 1)

  else:
    discard

  return moves

# ------------------------------
# UCI‑move conversion
# ------------------------------
proc uciMoveToMove(b: Board, uci: string): Option[Move] =
  if uci.len < 4:
    return none(Move)
  let fc = uci[0]
  let fr = uci[1]
  let tc = uci[2]
  let tr = uci[3]

  if not (fc >= 'a' and fc <= 'i' and fr >= '0' and fr <= '9' and
          tc >= 'a' and tc <= 'i' and tr >= '0' and tr <= '9'):
    return none(Move)

  let fromC = int(fc.ord - 'a'.ord)
  let fromR = 9 - int(fr.ord - '0'.ord)
  let toC   = int(tc.ord - 'a'.ord)
  let toR   = 9 - int(tr.ord - '0'.ord)

  if not (inBoard(fromR, fromC) and inBoard(toR, toC)):
    return none(Move)

  let pm = b.generatePieceMoves(fromR, fromC)
  for m in pm:
    if m.toR == toR and m.toC == toC:
      return some(m)
  none(Move)


proc generateAllMoves(b: Board): seq[Move] =
  var moves = newSeq[Move]()
  for r in 0..9:
    for c in 0..8:
      let p = b.board[r][c]
      if p == 0'u8:
        continue
      if b.redTurn and not isRed(p):
        continue
      if not b.redTurn and not isBlack(p):
        continue
      let pm = b.generatePieceMoves(r, c)
      moves.add(pm)
  return moves

proc makeMove(b: var Board, m: Move) =
  b.captured.add(b.board[m.toR][m.toC])
  b.board[m.toR][m.toC] = b.board[m.fromR][m.fromC]
  b.board[m.fromR][m.fromC] = EMPTY
  b.redTurn = not b.redTurn
  b.history.add(m)

# ------------------------------
# UCI Engine wrapper
# ------------------------------
type
  UciEngine* = object
    process: Process
    stdin: Stream
    reader: Stream

proc sendCmd(eng: var UciEngine, cmd: string) =
  eng.stdin.writeLine(cmd)
  eng.stdin.flush()

proc readStream(src: Stream): string = 
  var line = ""
  try:
    line = src.readLine()
  except EOFError:
    return ""
  except IOError:
    return ""
  
  return line.strip()

proc newUciEngine*(path: string): Result[UciEngine, string] =
  let p = startProcess(
    path,
    workingDir = "",
    args = @[],
    options = {poUsePath, poStdErrToStdOut}  # Redirection handled by options
  )

  if p.isNil:
    return Err[UciEngine, string]("cannot start engine: " & path) 

  var e: UciEngine = UciEngine(
    process: p,
    stdin: p.inputStream,
    reader: p.outputStream
  )

  sendCmd(e, "uci")
  var ok = false
  while true:
    let s = readStream(e.reader)
    if s != "" and s.find("uciok") != -1:
      ok = true
      break

  if not ok:
    return Err[UciEngine,string]("missing uciok")

  sendCmd(e, "isready")
  while true:
    let s = readStream(e.reader)
    if s.find("readyok") != -1:
      break

  return Ok[UciEngine, string](e)

proc getBestMove(eng: var UciEngine, posCmd: string, movetimeMs: uint64): Result[string, string] =
  eng.stdin.writeLine("ucinewgame")
  eng.stdin.writeLine(posCmd)
  eng.stdin.writeLine("go movetime " & $movetimeMs)
  eng.stdin.flush()

  while true:
    let ln = readStream(eng.reader)
    if ln.startsWith("bestmove"):
      let parts = ln.splitWhitespace()
      if parts.len >= 2:
        return Ok[string,string](parts[1])
      return Err[string,string]("no bestmove")

proc quit(eng: var UciEngine) =
  eng.stdin.writeLine("quit")
  try:
    eng.process.terminate()
    discard eng.process.waitForExit(timeout=1000)
    doAssert not eng.process.running
    eng.process.close()
  except IOError, OSError, ValueError:                         discard # Ignore errors during process cleanup

# ------------------------------
# Terminal board rendering
# ------------------------------
proc printBoard(b: Board, selR, selC: int; targets: seq[(int, int)]) =
  stdout.write("\x1b[2J\x1b[H")
  stdout.flushFile()
  echo()

  write(stdout, repeat(SPACE,8))
  for c in 0..8:
    write(stdout, $c & repeat(SPACE,4))
  echo()
  echo()

  for r in 0..9:
    write(stdout, repeat(SPACE,6) & $r & " ")
    for c in 0..8:
      let pc = b.board[r][c]
      let isSel = (r == selR and c == selC)
      let isTgt = containsTuple(targets, r, c)

      var cell = if pc == 0'u8: V_LINE else: PIECE_NAMES[pc.int]
      if isSel:
        cell = bgSelect(cell)
      elif isTgt:
        cell = bgTarget(cell)

      if pc != 0'u8:
        cell = if isRed(pc): red(cell) else: blue(cell)
      write(stdout, cell)

      if c < 8:
        if pc != 0'u8:
          write(stdout, repeat(H_LINE,3))
        else:
          write(stdout, repeat(H_LINE,4))
    echo()

    if r < 9:
      write(stdout, repeat(SPACE,8))
      if r == 4:
        echo V_LINE & repeat(SPACE,8) & "~~~~~" & repeat(SPACE,13) & "~~~~~" & repeat(SPACE,8) & V_LINE
        continue
      for c in 0..8:
        var cell = V_LINE & repeat(SPACE,4)
        if (c == 3 and (r == 0 or r == 7)) or (c == 4 and (r == 1 or r == 8)):
          cell = "│ " & B_SLASH & repeat(SPACE,2)
        elif (c == 3 and (r == 1 or r == 8)) or (c == 4 and (r == 0 or r == 7)):
          cell = "│ " & SLASH & repeat(SPACE,2)
        write(stdout, cell)
      echo()
      #echo()

  #acquire(ENGINE_MOVE_LOCK)
  echo "引擎: ", ENGINE_MOVE
  #release(ENGINE_MOVE_LOCK)

  if b.redTurn:
    echo green("      ► 轮到: 红方 (帅)")
  else:
    echo blue("      ► 轮到: 黑方 (将)")
  echo()
  stdout.flushFile()

proc coordInput(prompt: string): (int, int) =
  while true:
    echo("    <q:quit> " & prompt)
    stdout.flushFile()
    let input = stdin.readLine().strip()
    if input == "q":
      return (-1,-1)
    if input.len >= 2:
      let r = parseInt(input[0..0])
      let c = parseInt(input[1..1])
      if inBoard(r, c):
        return (r, c)

# ------------------------------
# Main game loop
# ------------------------------
proc main() =
  #initLock(#ENGINE_MOVE_LOCK)
  var board = newBoard()

  var outsideEngine: Option[UciEngine] = none(UciEngine)
  var engineTime: uint64 = 9000
  var playerIsRed: bool

  proc cleanup() =
    if outsideEngine.isSome:
      var e = outsideEngine.get()
      e.quit()
  defer: cleanup()

  echo()
  write(stdout,"  输入引擎路径 (如 ./pikafish): ")
  stdout.flushFile()
  let path = stdin.readLine().strip()
  let enginePath = if path.len == 0: "/data/data/com.termux/files/home/bin/pikafish" else: path

  write(stdout,"  引擎每步思考时间(ms) [默认9000]: ")
  stdout.flushFile()
  let timeStr = stdin.readLine().strip()
  if timeStr != "":
    engineTime = parseBiggestInt(timeStr).uint64

  write(stdout,"  玩家执红先走? (y/n) [默认y]: ")
  stdout.flushFile()
  let colorStr = stdin.readLine().strip().toLower()
  playerIsRed = colorStr.len == 0 or colorStr[0] == 'y'

  echo()
  echo("  正在启动外部引擎 ...")
  let engRes = newUciEngine(enginePath)
  if engRes.isOk:
    outsideEngine = some(engRes.get())
    echo("  ✓ 已连接！")
    echo("  玩家执", if playerIsRed: "红" else:"黑", ", 外部引擎执", if playerIsRed:"黑" else:"红")
  else:
    echo("  ✗ 无法启动外部引擎: ", engRes.error)
    echo("  请检查路径是否正确，按 Enter 退出...")
    discard stdin.readLine()
    return

  echo()
  while true:
    printBoard(board, -1, -1, @[])
    let moves = board.generateAllMoves()

    if moves.len == 0:
      if board.isInCheck(board.redTurn):
        if board.redTurn:
          echo blue("═══════════════════════════════════════")
          echo blue("         黑方胜利！将死红帅！")
          echo blue("═══════════════════════════════════════")
        else:
          echo red("═══════════════════════════════════════")
          echo red("         红方胜利！将死黑将！")
          echo red("═══════════════════════════════════════")
      else:
        echo yellow("═══════════════════════════════════════")
        echo yellow("              和棋！无子可动")
        echo yellow("═══════════════════════════════════════")
      echo()
      echo("按 Enter 退出...")
      discard stdin.readLine()
      break

    let isPlayer = (board.redTurn == playerIsRed)
    if isPlayer:
      var valid = false
      while not valid:
        let (fr, fc) = coordInput("  选择棋子 (行列如73): ")
        if (fr, fc) == (-1, -1): return
        let p = board.board[fr][fc]
        if p == 0'u8:
          echo("  ⚠ 此处无棋子！")
          continue
        if board.redTurn and not isRed(p):
          echo("  ⚠ 请选红方棋子！")
          continue
        if not board.redTurn and not isBlack(p):
          echo("  ⚠ 请选黑方棋子！")
          continue

        let pieceMoves = board.generatePieceMoves(fr, fc)
        if pieceMoves.len == 0:
          echo("  ⚠ 此棋子无合法走法！")
          continue

        var targets: seq[(int, int)]
        for m in pieceMoves:
          targets.add((m.toR, m.toC))
        printBoard(board, fr, fc, targets)

        echo("  可走位置:")
        for i, m in pieceMoves.pairs:
          write(stdout, "    ", $i,": (",$m.toR,",",$m.toC,")")
          if board.board[m.toR][m.toC] != 0'u8:
            write(stdout, " [吃", PIECE_NAMES[board.board[m.toR][m.toC].int],"]")
          if (i mod 2) == 1:
            echo()
        if (pieceMoves.len mod 2) != 0:
          echo()
        echo()

        let (tr, tc) = coordInput("  目标位置 (行列如64): ")
        var found = false
        for m in pieceMoves:
          if m.toR == tr and m.toC == tc:
            let cap = board.board[tr][tc]
            board.makeMove(m)
            printBoard(board, -1, -1, @[])
            if cap != 0'u8:
              echo("  ✓ 吃掉 ", PIECE_NAMES[cap.int],"!")
            found = true
            valid = true
            break
        if not found:
          echo("  ✗ 非法走法！")
    else:
      echo("  引擎思考中...")
      if outsideEngine.isSome:
        var eng = outsideEngine.get()
        let fen = board.toFen()
        let posCmd = "position fen " & fen
        let bmRes = eng.getBestMove(posCmd, engineTime)
        if bmRes.isOk:
          let uciMove = bmRes.get()
          let mo = board.uciMoveToMove(uciMove)
          if mo.isSome:
            let mv = mo.get()
            board.makeMove(mv)
            #acquire(ENGINE_MOVE_LOCK)
            ENGINE_MOVE = $mv.fromR & "," & $mv.fromC & "->" & $mv.toR & "," & $mv.toC
            #release(ENGINE_MOVE_LOCK)
            printBoard(board, -1, -1, @[])
          else:
            echo("  ✗ 引擎返回非法走法: ", uciMove)
            break
        else:
          echo("  ✗ 引擎通信错误: ", bmRes.error)
          break
      else:
        echo("  ✗ 引擎未连接！")
        break

  #if outsideEngine.isSome:
  #  var e = outsideEngine.get()
  #  e.quit()

when isMainModule:
  main()
