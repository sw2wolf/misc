;;;; xq-uci.lisp
;;;; Common Lisp (SBCL) Chinese chess (Xiangqi) + UCI engine frontend.
;;;; Faithful port of the Nim xq_uci.nim.
;;;;
;;;; Build:
;;;;   sbcl --noinform --non-interactive --load xq-uci.lisp --eval '(sb-ext:save-lisp-and-die "xq-uci" :executable t :toplevel (quote xq-uci::main) :purify t :compression t)'
;;;; Run:
;;;;   ./xq-uci

(defpackage #:xq-uci (:use #:cl))
(in-package #:xq-uci)

(declaim (optimize (speed 2) (safety 1) (debug 1)))

;;; ============================================================
;;; Piece constants
;;; ============================================================
(defconstant +empty+     0)
(defconstant +r-king+    1)
(defconstant +r-advisor+ 2)
(defconstant +r-bishop+  3)
(defconstant +r-knight+  4)
(defconstant +r-rook+    5)
(defconstant +r-cannon+  6)
(defconstant +r-pawn+    7)
(defconstant +b-king+    8)
(defconstant +b-advisor+ 9)
(defconstant +b-bishop+  10)
(defconstant +b-knight+  11)
(defconstant +b-rook+    12)
(defconstant +b-cannon+  13)
(defconstant +b-pawn+    14)

(defparameter *piece-names*
#("  " "帅" "仕" "相" "马" "车" "炮" "兵"
"将" "士" "象" "马" "车" "炮" "卒"))

(defparameter *esc* (code-char 27))

;;; ============================================================
;;; ANSI color helpers
;;; ============================================================
(defun red    (s) (format nil "~c[1;31m~a~c[0m" *esc* s *esc*))
(defun blue   (s) (format nil "~c[1;34m~a~c[0m" *esc* s *esc*))
(defun yellow (s) (format nil "~c[1;33m~a~c[0m" *esc* s *esc*))
(defun green  (s) (format nil "~c[1;32m~a~c[0m" *esc* s *esc*))
(defun bg-select (s) (format nil "~c[44;1;37m~a~c[0m" *esc* s *esc*))
(defun bg-target (s) (format nil "~c[42;1;30m~a~c[0m" *esc* s *esc*))

;;; ============================================================
;;; Board + move
;;; ============================================================
(defstruct board
  (cells (make-array '(10 9) :element-type '(unsigned-byte 8) :initial-element 0))
  (red-turn t)
  (captured nil))

(defstruct move from-r from-c to-r to-c (score 0))

(declaim (inline piece-at))
(defun piece-at (b r c) (aref (board-cells b) r c))

(defun is-red   (p) (<= 1 p 7))
(defun is-black (p) (<= 8 p 14))

(defun same-side (a b)
  (or (zerop a) (zerop b)
      (and (is-red a) (is-red b))
      (and (is-black a) (is-black b))))

(defun in-board (r c) (and (<= 0 r 9) (<= 0 c 8)))

(defun copy-array (arr)
  (let ((new (make-array (array-dimensions arr)
:element-type (array-element-type arr))))
    (loop for i below (array-total-size arr)
      do (setf (row-major-aref new i) (row-major-aref arr i)))
    new))

;(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
;copy-board will shadow the auto-generated name
;
(defun deep-copy-board (b)
  (make-board
    :cells   (copy-array (board-cells b))
    :red-turn (board-red-turn b)
    :captured (copy-list (board-captured b))))

(defun new-board ()
  (let ((b (make-board)))
    (flet ((put (r c p) (setf (aref (board-cells b) r c) p)))
      (put 9 0 +b-rook+) (put 9 1 +b-knight+) (put 9 2 +b-bishop+)
      (put 9 3 +b-advisor+) (put 9 4 +b-king+) (put 9 5 +b-advisor+)
      (put 9 6 +b-bishop+) (put 9 7 +b-knight+) (put 9 8 +b-rook+)
      (put 7 1 +b-cannon+) (put 7 7 +b-cannon+)
      (put 6 0 +b-pawn+) (put 6 2 +b-pawn+) (put 6 4 +b-pawn+)
      (put 6 6 +b-pawn+) (put 6 8 +b-pawn+)
      (put 3 0 +r-pawn+) (put 3 2 +r-pawn+) (put 3 4 +r-pawn+)
      (put 3 6 +r-pawn+) (put 3 8 +r-pawn+)
      (put 2 1 +r-cannon+) (put 2 7 +r-cannon+)
      (put 0 0 +r-rook+) (put 0 1 +r-knight+) (put 0 2 +r-bishop+)
      (put 0 3 +r-advisor+) (put 0 4 +r-king+) (put 0 5 +r-advisor+)
      (put 0 6 +r-bishop+) (put 0 7 +r-knight+) (put 0 8 +r-rook+)) b))

;;; ============================================================
;;; FEN
;;; ============================================================
(defun piece-char (p)
  (case p
    ((12) #\r) ((11) #\n) ((10) #\b) ((9) #\a) ((8) #\k)
    ((13) #\c) ((14) #\p)
    ((5) #\R)  ((4) #\N)  ((3) #\B)  ((2) #\A)  ((1) #\K)
    ((6) #\C)  ((7) #\P)
    (t #\?)))

(defun to-fen (b)
  (with-output-to-string (s)
    (loop for r from 9 downto 0 do
      (let ((empty-count 0))
        (loop for c from 0 to 8 do
          (let ((pc (piece-at b r c)))
            (if (zerop pc)
              (incf empty-count)
              (progn
                (when (> empty-count 0)
                  (princ empty-count s)
                  (setf empty-count 0))
                (write-char (piece-char pc) s)))))
        (when (> empty-count 0) (princ empty-count s)))
      (when (> r 0) (write-char #\/ s)))
    (write-string (if (board-red-turn b) " w - - 0 1" " b - - 0 1") s)))

;;; =======================================================
;;; King & check detection
;;; =======================================================
(defun find-king (b red-side)
  (let ((target (if red-side +r-king+ +b-king+)))
    (loop for r from 0 to 9 do
      (loop for c from 0 to 8 do
        (when (= (piece-at b r c) target)
          (return-from find-king (values r c)))))
    (values nil nil)))

(defun rook-attacks-p (b r c kr kc)
  (or (and (= r kr) (/= c kc)
        (let* ((s (signum (- kc c)))
          (dist (abs (- kc c))))
          (loop for d from 1 below dist
            always (zerop (piece-at b r (+ c (* s d)))))))
      (and (= c kc) (/= r kr)
        (let* ((s (signum (- kr r)))
               (dist (abs (- kr r))))
          (loop for d from 1 below dist
            always (zerop (piece-at b (+ r (* s d)) c)))))))

(defun knight-attacks-p (b r c kr kc)
  (let ((dr (abs (- r kr))) (dc (abs (- c kc))))
    (when (or (and (= dr 2) (= dc 1)) (and (= dr 1) (= dc 2)))
      (multiple-value-bind (br bc)
        (if (= dr 2)
          (values (floor (+ r kr) 2) c)
          (values r (floor (+ c kc) 2)))
        (and (in-board br bc) (zerop (piece-at b br bc)))))))

(defun cannon-attacks-p (b r c kr kc)
  (flet ((between-count-1 (xs) (= 1 (count-if-not #'zerop xs))))
    (or (and (= r kr) (/= c kc)
          (let* ((s (signum (- kc c)))
            (between 
              (loop for d from 1 below (abs (- kc c))
                  collect (piece-at b r (+ c (* s d))))))
            (between-count-1 between)))
        (and (= c kc) (/= r kr)
          (let* ((s (signum (- kr r)))
            (between 
              (loop for d from 1 below (abs (- kr r))
                collect (piece-at b (+ r (* s d)) c))))
            (between-count-1 between))))))

(defun red-pawn-attacks-p   (r c kr kc)
  (or (and (= (+ r 1) kr) (= c kc))
      (and (= r kr) (= (abs (- c kc)) 1) (>= r 5))))

(defun black-pawn-attacks-p (r c kr kc)
  (or (and (= (- r 1) kr) (= c kc))
        (and (= r kr) (= (abs (- c kc)) 1) (<= r 4))))

(defun king-attacks-p (b r c kr kc)
  (when (= c kc)
    (loop for rr from (1+ (min r kr)) below (max r kr)
      always (zerop (piece-at b rr c)))))

(defun attacks-p (b r c p kr kc)
  (cond
    ((or (= p +r-rook+) (= p +b-rook+))     (rook-attacks-p b r c kr kc))
    ((or (= p +r-knight+) (= p +b-knight+)) (knight-attacks-p b r c kr kc))
    ((or (= p +r-cannon+) (= p +b-cannon+)) (cannon-attacks-p b r c kr kc))
    ((= p +r-pawn+)                         (red-pawn-attacks-p r c kr kc))
    ((= p +b-pawn+)                         (black-pawn-attacks-p r c kr kc))
    ((or (= p +r-king+) (= p +b-king+))     (king-attacks-p b r c kr kc))
    (t nil)))

(defun is-in-check (b red-side)
  (block check
    (multiple-value-bind (kr kc) (find-king b red-side)
      (when (null kr) (return-from check nil))
      (loop for r from 0 to 9 do
        (loop for c from 0 to 8 do
          (let ((p (piece-at b r c)))
            (when (and (not (zerop p))
              (if red-side (is-black p) (is-red p)))
              (when (attacks-p b r c p kr kc)
                (return-from check t))))))
      nil)))

;;; =======================================================
;;; Move generation
;;; =======================================================
(defun add-if-valid (b piece r c tr tc moves)
  (if (or (not (in-board tr tc))
        (let ((tp (piece-at b tr tc)))
          (and (not (zerop tp)) (same-side piece tp))))
      moves
    (let ((ns (deep-copy-board b)))
      (setf (aref (board-cells ns) tr tc) piece)
      (setf (aref (board-cells ns) r c)   +empty+)
      (if (is-in-check ns (is-red piece))
        moves
        (cons (make-move :from-r r :from-c c :to-r tr :to-c tc) moves)))))

(defun king-moves (b p r c)
  (let ((moves nil))
    (dolist (d '((-1 0) (1 0) (0 -1) (0 1)) moves)
      (destructuring-bind (dr dc) d
        (let ((tr (+ r dr)) (tc (+ c dc)))
          (if (is-red p)
            (when (and (<= 0 tr 2) (<= 3 tc 5))
              (setf moves (add-if-valid b p r c tr tc moves)))
            (when (and (<= 7 tr 9) (<= 3 tc 5))
              (setf moves (add-if-valid b p r c tr tc moves)))))))))

(defun advisor-moves (b p r c)
  (let ((moves nil))
    (dolist (d '((-1 -1) (-1 1) (1 -1) (1 1)) moves)
      (destructuring-bind (dr dc) d
        (let ((tr (+ r dr)) (tc (+ c dc)))
          (if (is-red p)
            (when (and (<= 0 tr 2) (<= 3 tc 5))
              (setf moves (add-if-valid b p r c tr tc moves)))
            (when (and (<= 7 tr 9) (<= 3 tc 5))
              (setf moves (add-if-valid b p r c tr tc moves)))))))))

(defun bishop-moves (b p r c)
  (let ((moves nil))
    (dolist (d '((-2 -2) (-2 2) (2 -2) (2 2)) moves)
      (destructuring-bind (dr dc) d
        (let ((br (+ r (floor dr 2)))
              (bc (+ c (floor dc 2)))
              (tr (+ r dr))
              (tc (+ c dc)))
          (when (and (in-board br bc) (zerop (piece-at b br bc)))
            (if (is-red p)
              (when (<= 0 tr 4)
                (setf moves (add-if-valid b p r c tr tc moves)))
              (when (<= 5 tr 9)
                (setf moves (add-if-valid b p r c tr tc moves))))))))))

(defun knight-moves (b p r c)
  (let ((moves nil))
    (dolist (d '((-2 -1) (-2 1) (-1 -2) (-1 2)
(1 -2)  (1 2)  (2 -1)  (2 1)) moves)
        (destructuring-bind (dr dc) d
          (let ((tr (+ r dr)) (tc (+ c dc)))
            (when (in-board tr tc)
              (multiple-value-bind (br bc)
                (if (= (abs dr) 2)
                  (values (+ r (floor dr 2)) c)
                  (values r (+ c (floor dc 2))))
                (when (and (in-board br bc) (zerop (piece-at b br bc)))
                  (setf moves (add-if-valid b p r c tr tc moves))))))))))

(defun rook-moves (b p r c)
  (let ((moves nil))
    (flet ((slide (dr dc)
      (loop for tr = (+ r dr) then (+ tr dr)
            for tc = (+ c dc) then (+ tc dc)
            while (in-board tr tc) do
              (let ((tp (piece-at b tr tc)))
                (cond
                  ((zerop tp)
                    (setf moves (add-if-valid b p r c tr tc moves)))
                  ((same-side p tp) (return))
                  (t
                    (setf moves (add-if-valid b p r c tr tc moves))
                    (return)))))))
        (slide -1 0) (slide 1 0) (slide 0 -1) (slide 0 1))
moves))

(defun cannon-moves (b p r c)
  (let ((moves nil))
    (flet ((slide (dr dc)
      (let ((tr (+ r dr)) (tc (+ c dc)))
;; slide over empties (no capture)
        (loop while (and (in-board tr tc)
          (zerop (piece-at b tr tc)))
          do (setf moves (add-if-valid b p r c tr tc moves))
            (incf tr dr) (incf tc dc))
        ;; skip screen
        (when (in-board tr tc)
          (incf tr dr) (incf tc dc)
          (loop while (and (in-board tr tc)
            (zerop (piece-at b tr tc)))
              do (incf tr dr) (incf tc dc))
          (when (in-board tr tc)
            (let ((tp (piece-at b tr tc)))
              (unless (same-side p tp)
              (setf moves (add-if-valid b p r c tr tc moves)))))))))
        (slide -1 0) (slide 1 0) (slide 0 -1) (slide 0 1))
moves))

(defun pawn-moves (b p r c)
  (let ((moves nil))
    (if (is-red p)
      (progn
        (setf moves (add-if-valid b p r c (+ r 1) c moves))
        (when (>= r 5)
          (setf moves (add-if-valid b p r c r (- c 1) moves))
          (setf moves (add-if-valid b p r c r (+ c 1) moves))))
      (progn
        (setf moves (add-if-valid b p r c (- r 1) c moves))
        (when (<= r 4)
          (setf moves (add-if-valid b p r c r (- c 1) moves))
          (setf moves (add-if-valid b p r c r (+ c 1) moves)))))
moves))

(defun generate-piece-moves (b r c)
  (let ((p (piece-at b r c)))
    (cond
      ((zerop p)                     nil)
      ((or (= p +r-king+)   (= p +b-king+))   (king-moves    b p r c))
      ((or (= p +r-advisor+) (= p +b-advisor+))(advisor-moves b p r c))
      ((or (= p +r-bishop+) (= p +b-bishop+)) (bishop-moves  b p r c))
      ((or (= p +r-knight+) (= p +b-knight+)) (knight-moves  b p r c))
      ((or (= p +r-rook+)   (= p +b-rook+))   (rook-moves    b p r c))
      ((or (= p +r-cannon+) (= p +b-cannon+)) (cannon-moves  b p r c))
      ((or (= p +r-pawn+)   (= p +b-pawn+))   (pawn-moves    b p r c))
      (t nil))))

(defun generate-all-moves (b)
  (let ((moves nil)
    (turn-red (board-red-turn b)))
    (dotimes (r 10)
      (dotimes (c 9)
        (let ((p (piece-at b r c)))
          (when (and (not (zerop p))
            (if turn-red (is-red p) (is-black p)))
            (dolist (m (generate-piece-moves b r c))
              (push m moves))))))
    (nreverse moves)))

(defun apply-move (b m)
"Destructively mutate board b by playing m. Returns b."
  (push (piece-at b (move-to-r m) (move-to-c m))
    (board-captured b))
  (setf (aref (board-cells b) (move-to-r m) (move-to-c m))
    (piece-at b (move-from-r m) (move-from-c m)))
  (setf (aref (board-cells b) (move-from-r m) (move-from-c m)) +empty+)
  (setf (board-red-turn b) (not (board-red-turn b)))
b)

;;; =======================================================
;;; UCI coordinate <-> Move
;;; =======================================================
(defun uci-move-to-move (b uci)
  (when (>= (length uci) 4)
    (let ((fc (char uci 0)) (fr (char uci 1))
      (tc (char uci 2)) (tr (char uci 3)))
      (when (and (char<= #\a fc #\i)
        (char<= #\0 fr #\9)
        (char<= #\a tc #\i)
        (char<= #\0 tr #\9))
        (let ((from-c (- (char-code fc) (char-code #\a)))
          (from-r (- (char-code fr) (char-code #\0)))
          (to-r   (- (char-code tr) (char-code #\0)))
          (to-c   (- (char-code tc) (char-code #\a))))
          (when (and (in-board from-r from-c) (in-board to-r to-c))
            (find-if (lambda (m)
              (and (= (move-to-r m) to-r)
                (= (move-to-c m) to-c)))
              (generate-piece-moves b from-r from-c))))))))

;;; =======================================================
;;; UCI engine wrapper
;;; =======================================================
(defstruct uci-engine
  process
  stdin
  stdout)

(defun send-cmd (eng cmd)
(format (uci-engine-stdin eng) "~a~%" cmd)
(finish-output (uci-engine-stdin eng)))

(defun read-line-safe (stream)
  (handler-case (read-line stream nil nil)
    (error () nil)))

(defun wait-for-token (eng token)
  (loop
    (let ((line (read-line-safe (uci-engine-stdout eng))))
      (when (null line) (return nil))
      (when (search token line) (return t)))))

(defun split-whitespace (s)
  (let ((out nil) (n (length s)) (i 0))
    (loop
      (loop while (and (< i n) (member (char s i)
'(#\Space #\Tab #\Newline #\Return)))
        do (incf i))
          (when (>= i n) (return))
          (let ((start i))
          (loop while (and (< i n) (not (member (char s i)
'(#\Space #\Tab #\Newline #\Return))))
            do (incf i))
          (push (subseq s start i) out)))
      (nreverse out)))

(defun new-uci-engine (path)
"Returns (values T engine) or (values NIL error-string)."
  (handler-case
    (let* ((proc (sb-ext:run-program path nil
            :input  :stream
            :output :stream
            :error  :output
            :wait   nil
            :search t))
      (eng (make-uci-engine
        :process proc
        :stdin  (sb-ext:process-input  proc)
        :stdout (sb-ext:process-output proc))))
      (send-cmd eng "uci")
      (if (not (wait-for-token eng "uciok"))
        (values nil "missing uciok")
        (progn
          (send-cmd eng "isready")
          (wait-for-token eng "readyok")
            (values t eng))))
    (error (e)
      (values nil (format nil "cannot start engine: ~a (~a)" path e)))))

(defun get-best-move (eng pos-cmd movetime-ms)
  (send-cmd eng "ucinewgame")
  (send-cmd eng pos-cmd)
  (send-cmd eng (format nil "go movetime ~a" movetime-ms))
  (loop
    (let ((ln (read-line-safe (uci-engine-stdout eng))))
      (when (null ln) (return (values nil "EOF while waiting for bestmove")))
      (when (and (>= (length ln) 8)
        (string= "bestmove" ln :end2 8))
        (let ((parts (split-whitespace ln)))
          (if (>= (length parts) 2)
            (return (values t (second parts)))
            (return (values nil "no bestmove in reply"))))))))

(defun quit-engine (eng)
  (ignore-errors
    (format (uci-engine-stdin eng) "quit~%")
    (finish-output (uci-engine-stdin eng)))
  (ignore-errors (sb-ext:process-kill (uci-engine-process eng) 15))
  (ignore-errors (sb-ext:process-wait (uci-engine-process eng)))
  (ignore-errors (sb-ext:process-close (uci-engine-process eng))))

;;; =======================================================
;;; Terminal rendering
;;; =======================================================
(defun contains-tuple (targets r c)
  (member (cons r c) targets :test #'equal))

(defun print-board (b sel-r sel-c targets engine-move-str)
  (format t "~c[2J~c[H" *esc* *esc*)
  (finish-output)
  (terpri)
  (loop for r from 9 downto 0 do
    (dotimes (_ 6) (princ " "))
    (format t "~a " r)
    (loop for c from 0 to 8 do
      (let* ((pc      (piece-at b r c))
            (is-sel  (and (= r sel-r) (= c sel-c)))
            (is-tgt  (contains-tuple targets r c))
            (base    (if (zerop pc) "│" (aref *piece-names* pc)))
            (with-bg (cond (is-sel (bg-select base))
                        (is-tgt (bg-target base))
                        (t      base)))
            (colored (if (not (zerop pc))
                      (if (is-red pc) (red with-bg) (blue with-bg)) with-bg)))
        (princ colored)
        (when (< c 8)
          (if (not (zerop pc))
            (princ "───")
            (princ "────")))))
    (terpri)
    (when (> r 0)
      (dotimes (_ 8) (princ " "))
      (if (= r 5)
        (progn
          (princ "│        ~~~~~             ~~~~~        │")
          (terpri))
        (progn
          (loop for c from 0 to 8 do
            (princ
              (cond
                ((or (and (= c 3) (or (= r 9) (= r 2)))
(and (= c 4) (or (= r 8) (= r 1)))) "│ ╲  ")
                ((or (and (= c 3) (or (= r 8) (= r 1)))
(and (= c 4) (or (= r 9) (= r 2)))) "│ ╱  ")
                (t "│    "))))
          (terpri)))))
  (dotimes (_ 8) (princ " "))
  (loop for c from 0 to 8 do (format t "~a    " c))
    (terpri)
    (terpri)
    (format t "引擎: ~a~%" engine-move-str)
    (if (board-red-turn b)
      (format t "~a~%" (green "      ► 轮到: 红方 (帅)"))
      (format t "~a~%" (blue  "      ► 轮到: 黑方 (将)")))
    (terpri)
    (finish-output))

;;; =======================================================
;;; Input
;;; =======================================================
(defun strip-line (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun coord-input (prompt)
"Returns (values r c) on success, (values nil nil) for 'q'."
  (loop
    (format t "    <q:quit> ~a~%" prompt)
    (finish-output)
    (let ((input (strip-line (or (read-line *standard-input* nil "") ""))))
      (cond
        ((string= input "q")
          (return-from coord-input (values nil nil)))
        ((>= (length input) 2)
          (let ((r (digit-char-p (char input 0)))
            (c (digit-char-p (char input 1))))
            (when (and r c (in-board r c))
              (return-from coord-input (values r c)))))))))

(defun select-piece (b)
"Returns (values r c) or (values nil nil) for quit."
  (loop
    (multiple-value-bind (fr fc)
      (coord-input "  选择棋子 (行列如32): ")
      (when (null fr) (return (values nil nil)))
        (let ((p (piece-at b fr fc)))
          (cond
            ((zerop p)
              (princ "  ⚠ 此处无棋子！") (terpri))
            ((and (board-red-turn b) (not (is-red p)))
              (princ "  ⚠ 请选红方棋子！") (terpri))
            ((and (not (board-red-turn b)) (not (is-black p)))
              (princ "  ⚠ 请选黑方棋子！") (terpri))
            (t
              (let ((pm (generate-piece-moves b fr fc)))
                (if (null pm)
                  (progn (princ "  ⚠ 此棋子无合法走法！") (terpri))
                  (return (values fr fc))))))))))

;;; =======================================================
;;; Turn handlers
;;; =======================================================
(defun player-turn (b engine-move-str)
"Mutates b on success; returns b, or NIL for quit."
  (block player-turn
  (loop
    (multiple-value-bind (fr fc) (select-piece b)
      (when (null fr) (return-from player-turn nil))
        (let* ((piece-moves (generate-piece-moves b fr fc))
              (targets (mapcar (lambda (m)
                (cons (move-to-r m) (move-to-c m))) piece-moves)))
          (print-board b fr fc targets engine-move-str)
          (princ "  可走位置:") (terpri)
          (loop for i from 0
            for m in piece-moves do
              (format t "    ~a: (~a,~a)" i (move-to-r m) (move-to-c m))
              (let ((tc (piece-at b (move-to-r m) (move-to-c m))))
                (when (not (zerop tc))
                  (format t " [吃~a]" (aref *piece-names* tc))))
                (when (oddp i) (terpri)))
          (when (oddp (length piece-moves)) (terpri))
          (terpri)
          (multiple-value-bind (tr tc)
            (coord-input "  目标位置 (行列如64): ")
            (when (null tr) (return-from player-turn nil))
            (let ((found (find-if (lambda (m)
              (and (= (move-to-r m) tr)
(= (move-to-c m) tc))) piece-moves)))
              (if found
                (let ((cap (piece-at b tr tc)))
                  (apply-move b found)
                  (when (not (zerop cap))
                    (format t "  ✓ 吃掉 ~a!~%" (aref *piece-names* cap)))
                    (return-from player-turn b))
                  (progn
                    (princ "  ✗ 非法走法！") (terpri))))))))))

(defun engine-turn (b eng engine-time)
"Mutates b on success; returns (values b t new-engine-str) or (values nil nil nil)."
  (princ "  引擎思考中...") (terpri)
  (let* ((fen     (to-fen b))
    (pos-cmd (concatenate 'string "position fen " fen)))
    (multiple-value-bind (ok? val) (get-best-move eng pos-cmd engine-time)
      (if ok?
        (let ((mv (uci-move-to-move b val)))
          (if mv
            (progn
              (apply-move b mv)
              (values b t
                (format nil "~a,~a->~a,~a" (move-from-r mv) (move-from-c mv) (move-to-r mv) (move-to-c mv))))
            (progn
              (format t "  ✗ 引擎返回非法走法: ~a~%" val)
              (values nil nil nil))))
        (progn
          (format t "  ✗ 引擎通信错误: ~a~%" val)
          (values nil nil nil))))))

;;; =======================================================
;;; Game loop
;;; =======================================================
(defun game-loop (b eng engine-time player-is-red)
  (let ((engine-move-str ""))
  (loop
    (print-board b -1 -1 nil engine-move-str)
    (let ((moves (generate-all-moves b)))
      (if (null moves)
        (progn
          (cond
            ((is-in-check b (board-red-turn b))
              (if (board-red-turn b)
                (progn
                  (format t "~a~%" (blue "═══════════════════════════════════════"))
                  (format t "~a~%" (blue "         黑方胜利！将死红帅！"))
                  (format t "~a~%" (blue "═══════════════════════════════════════")))
                (progn
                  (format t "~a~%" (red "═══════════════════════════════════════"))
                  (format t "~a~%" (red "         红方胜利！将死黑将！"))
                  (format t "~a~%" (red "═══════════════════════════════════════")))))
                (t
                  (format t "~a~%" (yellow "═══════════════════════════════════════"))
                  (format t "~a~%" (yellow "              和棋！无子可动"))
                  (format t "~a~%" (yellow "═══════════════════════════════════════"))))
          (terpri)
          (princ "按 Enter 退出...") (finish-output)
          (read-line)
          (return))

        (if (eql (board-red-turn b) player-is-red)
          (let ((new-b (player-turn b engine-move-str)))
(if new-b (setf b new-b) (return)))
            (multiple-value-bind (new-b ok? new-str)
              (engine-turn b eng engine-time)
              (if ok?
                (progn (setf b new-b)
                  (setf engine-move-str new-str))
                (return)))))))))

;;; =======================================================
;;; Main
;;; =======================================================
(defun main ()
  (let ((board (new-board))
    (eng   nil)
    (engine-time 9000)
    (player-is-red t))
    (unwind-protect
      (progn
        (terpri)
        (princ "  输入引擎路径 (如 ./pikafish): ") (finish-output)
        (let ((path (strip-line (or (read-line *standard-input* nil "") ""))))
          (when (string= path "")
            (setf path "/data/data/com.termux/files/home/bin/pikafish"))

          (princ "  引擎每步思考时间(ms) [默认9000]: ") (finish-output)
          (let ((ts (strip-line (or (read-line *standard-input* nil "") ""))))
            (unless (string= ts "")
              (setf engine-time
                (or (ignore-errors
                  (parse-integer ts :junk-allowed t))
                  9000))))

          (princ "  玩家执红先走? (y/n) [默认y]: ") (finish-output)
          (let ((cs (string-downcase
            (strip-line (or (read-line *standard-input* nil "") "")))))
            (setf player-is-red
              (or (string= cs "") (char= (char cs 0) #\y))))

          (terpri)
          (princ "  正在启动外部引擎 ...") (terpri)
          (multiple-value-bind (ok? val) (new-uci-engine path)
            (if ok?
              (progn
                (setf eng val)
                (princ "  ✓ 已连接！") (terpri)
                (format t "  玩家执~a, 外部引擎执~a~%"
                  (if player-is-red "红" "黑")
                    (if player-is-red "黑" "红"))
                (terpri)
                (game-loop board eng engine-time player-is-red))
              (progn
                (format t "  ✗ 无法启动外部引擎: ~a~%" val)
                (princ "  请检查路径是否正确，按 Enter 退出...")
                (terpri)
                (read-line))))))
      (when eng (quit-engine eng)))))
