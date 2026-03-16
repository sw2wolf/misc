import nimchess
import std/[strutils, strformat, options]

const
  ESC = "\e"
  CLEAR_SCREEN = ESC & "[2J"
  HIDE_CURSOR = ESC & "[?25l"
  SHOW_CURSOR = ESC & "[?25h"
  RESET = ESC & "[0m"

  # Foreground colors
  FG_BLACK = ESC & "[30m"
  FG_RED = ESC & "[31m"
  FG_GREEN = ESC & "[32m"
  FG_YELLOW = ESC & "[33m"
  FG_BLUE = ESC & "[34m"
  FG_MAGENTA = ESC & "[35m"
  FG_CYAN = ESC & "[36m"
  FG_WHITE = ESC & "[37m"

  # Background colors
  BG_BLACK = ESC & "[40m"
  BG_RED = ESC & "[41m"
  BG_GREEN = ESC & "[42m"
  BG_YELLOW = ESC & "[43m"
  BG_BLUE = ESC & "[44m"
  BG_MAGENTA = ESC & "[45m"
  BG_CYAN = ESC & "[46m"
  BG_WHITE = ESC & "[47m"

var engineOut = ""

proc mvToRowCol(row, col: int): string =
  ESC & "[" & $row & ";" & $col & "H"

func `$`(coloredPiece: ColoredPiece): string =
  const t = [
    white: [
      pawn: "♙", knight: "♘", bishop: "♗", rook: "♖", queen: "♕", king: "♔"
    ],
    black: [
      pawn: "♟", knight: "♞", bishop: "♝", rook: "♜", queen: "♛", king: "♚"
    ],

  ]
  if coloredPiece.piece == noPiece:
    return " "
  return t[coloredPiece.color][coloredPiece.piece]

proc showBoard(pos: Position) =
  # Clear screen and hide cursor for clean display
  stdout.write CLEAR_SCREEN
  stdout.write HIDE_CURSOR

  # Board coordinates
  let files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
  let ranks = ['1', '2', '3', '4', '5', '6', '7', '8']

  # Draw the board
  for rank in countdown(7, 0):
    # Rank label on the left
    stdout.write mvToRowCol(7-rank+1, 1)
    stdout.write FG_YELLOW & ranks[rank] & RESET & " "

    for file in 0..7:
      let sqare = (8 * rank + file).Square
      let piece = pos.pieceAt(sqare)
      let pieceChar = $pos.coloredPieceAt(sqare)

      # Determine square color (light or dark)
      let isLightSquare = (rank + file) mod 2 == 0
      # Set background color based on square color
      let bgColor = if isLightSquare: "\e[48;5;180m" else: "\e[48;5;94m"
      let fgColor = FG_BLACK

      # Draw the square with piece
      stdout.write bgColor & fgColor & " " & pieceChar & " " & RESET

    # Rank label on the right
    stdout.write " " & FG_YELLOW & ranks[rank] & RESET

  # File labels at the bottom
  stdout.write mvToRowCol(9, 3)
  for file in 0..7:
    stdout.write " " & FG_YELLOW & $files[file] & RESET & " "
  #show engine output
  if engineOut != "":
    stdout.write mvToRowCol(11, 1)
    stdout.write FG_CYAN & engineOut & RESET
    engineOut = ""

  # Optional: Show side to move
  stdout.write mvToRowCol(13, 1)
  stdout.write FG_CYAN & "To move: " & RESET
  # Assuming pos.sideToMove exists
  stdout.write (if pos.us == white: "White" else: "Black")

  # Move cursor to bottom and show cursor again
  stdout.write mvToRowCol(15, 1)
  stdout.flushFile()
  stdout.write SHOW_CURSOR

proc printResult(result: PlayResult, pos: Position) = 
  let engineMove = result.move
  let score = if result.info.score.isSome: result.info.score.get() else: Score(kind: skCp, cp: 0)
 
  engineOut = fmt"Engine plays: {engineMove.toUCI(pos)} ({engineMove.toSAN(pos)}) [eval: {$score}]"
  # Option 1: Create a centipawn score of 0
  #let score = Score(kind: skCp, cp: 0)

  # Option 2: If you want a mate score (though 0 mate doesn't make sense)
  #let score = Score(kind: skMate, mate: 0)
  #stdout.write mvToRowCol(13, 1)
  #stdout.write fmt"Engine plays: {engineMove.toUCI(pos)} ({engineMove.toSAN(pos)}) [eval: {$score}]"
  #stdout.flushFile()

proc main =
  stdout.write "♔♕♖♗♘♙ NIM CHESS ENGINE INTERFACE ♟♞♝♜♛♚\n"
  stdout.write "Starting Stockfish engine..."
  var engine = newUciEngine("stockfish")
  defer: engine.quit()

  # Set hash to 1024 MB (1GB) before playing
  engine.setOption("Hash", "1024")
  # Use 4 CPU threads
  engine.setOption("Threads", "4")

  # Let user choose color
  var userIsWhite = true
  while true:
    stdout.write "Do you want to play as White (w) or Black (b)? "
    stdout.flushFile()
    let choice = readLine(stdin).strip.toLowerAscii
    if choice == "w" or choice == "white":
      userIsWhite = true
      break
    elif choice == "b" or choice == "black":
      userIsWhite = false
      break
    else:
      stdout.write "Please enter 'w' or 'b'"

    # Starting position
  var pos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".toPosition

  stdout.write "\nEnter moves in UCI format (e2e4) or SAN (e4)."
  stdout.write "\nCommands: 'quit', 'board' (show board), 'moves' (list legal moves)"
  stdout.flushFile()

  # If user is Black, engine makes first move
  if not userIsWhite:
    stdout.write "Engine thinking..."
    stdout.flushFile()
    #let result = engine.play(pos, Limit(depth: 15))
    # Let the engine think for 5 seconds per move
    let result = engine.play(pos, Limit(movetimeSeconds: 5))

    printResult(result, pos)
    pos = pos.doMove(result.move)

  while true:
    #echo pos
    showBoard(pos)
    # Check if game is over
    if pos.isMate():
      if pos.us == white:
        stdout.write "Checkmate! Black wins!"
      else:
        stdout.write "Checkmate! White wins!"
      break
    elif pos.isStalemate():
      stdout.write "Stalemate! Game drawn."
      break

    # Determine whose turn it is
    let isUserTurn = (pos.us == white and userIsWhite) or (pos.us == black and not userIsWhite)

    if isUserTurn:
      stdout.write "Your move > "
      stdout.flushFile
      let userInput = readLine(stdin).strip

      if userInput.toLower == "quit":
        stdout.write "Game ended. Goodbye!\n"
        break

      if userInput.toLower == "board":
        continue

      if userInput.toLower == "moves":
        stdout.write "Legal moves:"
        for move in pos.legalMoves:
          stdout.write "  ", move.toUCI(pos), " (", move.toSAN(pos), ")"
        continue

      # Parse user move
      try:
        let userMove = userInput.toMove(pos)
        if not pos.isLegal(userMove):
          stdout.write "Illegal move! Try 'moves' to see legal moves."
          continue

        # Make the move
        pos = pos.doMove(userMove)
      except:
        stdout.write "Invalid format. Use UCI (e.g., 'e2e4')"
        continue

    else:
      # Engine's turn
      stdout.write "Engine thinking..."
      stdout.flushFile()
      #let result = engine.play(pos, Limit(depth: 15))
      let result = engine.play(pos, Limit(movetimeSeconds: 5))
      printResult(result, pos)
      pos = pos.doMove(result.move)


when isMainModule:
  main()

