
use std::io::{self, Write, BufRead, BufReader};
use std::time::{Duration, Instant};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::process::{Command, Child, ChildStdin, ChildStdout, Stdio};

// ============================================================
// 中国象棋引擎 - Rust实现 (UCI + 终端双模式) v2.3
// 新增: UCI协议支持，可与Pikafish等GUI对弈
// ============================================================

//const EMPTY: u8 = 0;
const R_KING: u8 = 1;
const R_ADVISOR: u8 = 2;
const R_BISHOP: u8 = 3;
const R_KNIGHT: u8 = 4;
const R_ROOK: u8 = 5;
const R_CANNON: u8 = 6;
const R_PAWN: u8 = 7;
const B_KING: u8 = 8;
const B_ADVISOR: u8 = 9;
const B_BISHOP: u8 = 10;
const B_KNIGHT: u8 = 11;
const B_ROOK: u8 = 12;
const B_CANNON: u8 = 13;
const B_PAWN: u8 = 14;

const PIECE_NAMES: [&str; 15] = [
    "  ", "帅", "仕", "相", "马", "车", "炮", "兵",
    "将", "士", "象", "马", "车", "炮", "卒"
];

fn red(s: &str) -> String { format!("\x1b[1;31m{}\x1b[0m", s) }
fn blue(s: &str) -> String { format!("\x1b[1;34m{}\x1b[0m", s) }
fn yellow(s: &str) -> String { format!("\x1b[1;33m{}\x1b[0m", s) }
fn green(s: &str) -> String { format!("\x1b[1;32m{}\x1b[0m", s) }
fn bg_select(s: &str) -> String { format!("\x1b[44;1;37m{}\x1b[0m", s) }
fn bg_target(s: &str) -> String { format!("\x1b[42;1;30m{}\x1b[0m", s) }

#[inline]
fn is_red(p: u8) -> bool { p >= 1 && p <= 7 }
#[inline]
fn is_black(p: u8) -> bool { p >= 8 && p <= 14 }
#[inline]
fn same_side(a: u8, b: u8) -> bool {
    a == 0 || b == 0 || (is_red(a) && is_red(b)) || (is_black(a) && is_black(b))
}
#[inline]
fn in_board(r: i32, c: i32) -> bool { r >= 0 && r <= 9 && c >= 0 && c <= 8 }

#[derive(Clone, Copy, Debug, PartialEq)]
struct Move {
    from_r: i32, from_c: i32, to_r: i32, to_c: i32, score: i32,
}

impl Move {
    fn new(fr: i32, fc: i32, tr: i32, tc: i32) -> Self {
        Move { from_r: fr, from_c: fc, to_r: tr, to_c: tc, score: 0 }
    }
}

#[derive(Clone)]
struct Board {
    board: [[u8; 9]; 10],
    red_turn: bool,
    history: Vec<Move>,
    captured: Vec<u8>,
}

impl Board {
    fn new() -> Self {
        let mut b = [[0u8; 9]; 10];
        b[0] = [B_ROOK, B_KNIGHT, B_BISHOP, B_ADVISOR, B_KING, B_ADVISOR, B_BISHOP, B_KNIGHT, B_ROOK];
        b[2] = [0, B_CANNON, 0, 0, 0, 0, 0, B_CANNON, 0];
        b[3] = [B_PAWN, 0, B_PAWN, 0, B_PAWN, 0, B_PAWN, 0, B_PAWN];
        b[6] = [R_PAWN, 0, R_PAWN, 0, R_PAWN, 0, R_PAWN, 0, R_PAWN];
        b[7] = [0, R_CANNON, 0, 0, 0, 0, 0, R_CANNON, 0];
        b[9] = [R_ROOK, R_KNIGHT, R_BISHOP, R_ADVISOR, R_KING, R_ADVISOR, R_BISHOP, R_KNIGHT, R_ROOK];
        Board { board: b, red_turn: true, history: Vec::new(), captured: Vec::new() }
    }

    // ==================== FEN 解析 / 生成 ====================
    fn from_fen(fen: &str) -> Result<Self, String> {
        let mut board = [[0u8; 9]; 10];
        let parts: Vec<&str> = fen.split_whitespace().collect();
        if parts.is_empty() { return Err("Empty FEN".to_string()); }

        let rows: Vec<&str> = parts[0].split('/').collect();
        if rows.len() != 10 { return Err(format!("Expected 10 rows, got {}", rows.len())); }

        for (r, row) in rows.iter().enumerate() {
            let mut c = 0usize;
            for ch in row.chars() {
                if c >= 9 { break; }
                if ch.is_ascii_digit() {
                    let empty_count = ch.to_digit(10).unwrap() as usize;
                    c += empty_count;
                } else {
                    let piece = match ch {
                        'r' => B_ROOK, 'n' => B_KNIGHT, 'b' => B_BISHOP,
                        'a' => B_ADVISOR, 'k' => B_KING, 'c' => B_CANNON, 'p' => B_PAWN,
                        'R' => R_ROOK, 'N' => R_KNIGHT, 'B' => R_BISHOP,
                        'A' => R_ADVISOR, 'K' => R_KING, 'C' => R_CANNON, 'P' => R_PAWN,
                        _ => return Err(format!("Invalid piece char: {}", ch)),
                    };
                    board[r][c] = piece;
                    c += 1;
                }
            }
        }

        let red_turn = if parts.len() > 1 {
            parts[1] == "w" || parts[1] == "r" || parts[1] == "W" || parts[1] == "R"
        } else {
            true
        };

        Ok(Board {
            board,
            red_turn,
            history: Vec::new(),
            captured: Vec::new(),
        })
    }

    fn to_fen(&self) -> String {
        let mut fen = String::new();
        for r in 0..10 {
            let mut empty_count = 0;
            for c in 0..9 {
                if self.board[r][c] == 0 {
                    empty_count += 1;
                } else {
                    if empty_count > 0 {
                        fen.push_str(&empty_count.to_string());
                        empty_count = 0;
                    }
                    let ch = match self.board[r][c] {
                        B_ROOK => 'r', B_KNIGHT => 'n', B_BISHOP => 'b',
                        B_ADVISOR => 'a', B_KING => 'k', B_CANNON => 'c', B_PAWN => 'p',
                        R_ROOK => 'R', R_KNIGHT => 'N', R_BISHOP => 'B',
                        R_ADVISOR => 'A', R_KING => 'K', R_CANNON => 'C', R_PAWN => 'P',
                        _ => '?',
                    };
                    fen.push(ch);
                }
            }
            if empty_count > 0 {
                fen.push_str(&empty_count.to_string());
            }
            if r < 9 { fen.push('/'); }
        }
        fen.push_str(if self.red_turn { " w - - 0 1" } else { " b - - 0 1" });
        fen
    }

    // UCI走法转换 (如 h2e2)
    fn uci_move_to_move(&self, uci: &str) -> Option<Move> {
        if uci.len() < 4 { return None; }
        let bytes = uci.as_bytes();
        let fc = bytes[0];
        let fr = bytes[1];
        let tc = bytes[2];
        let tr = bytes[3];

        if fc < b'a' || fc > b'i' || fr < b'0' || fr > b'9' ||
           tc < b'a' || tc > b'i' || tr < b'0' || tr > b'9' {
            return None;
        }

        let from_c = (fc - b'a') as i32;
        let from_r = 9 - (fr - b'0') as i32;
        let to_c = (tc - b'a') as i32;
        let to_r = 9 - (tr - b'0') as i32;

        if !in_board(from_r, from_c) || !in_board(to_r, to_c) { return None; }

        let piece_moves = self.generate_piece_moves(from_r, from_c);
        for m in &piece_moves {
            if m.to_r == to_r && m.to_c == to_c {
                return Some(*m);
            }
        }
        None
    }

    fn find_king(&self, red: bool) -> (i32, i32) {
        let target = if red { R_KING } else { B_KING };
        for r in 0..10 { for c in 0..9 { if self.board[r][c] == target { return (r as i32, c as i32); } } }
        (-1, -1)
    }

    fn is_in_check(&self, red: bool) -> bool {
        let (kr, kc) = self.find_king(red);
        if kr < 0 { return false; }
        let (kr, kc) = (kr as usize, kc as usize);
        for r in 0..10 {
            for c in 0..9 {
                let p = self.board[r][c];
                if p == 0 { continue; }
                if red && is_red(p) { continue; }
                if !red && is_black(p) { continue; }
                match p {
                    5 | 12 => {
                        if r == kr && c != kc {
                            let step = if c < kc { 1i32 } else { -1i32 };
                            let mut cc = c as i32 + step;
                            let mut blocked = false;
                            while cc != kc as i32 {
                                if self.board[r][cc as usize] != 0 { blocked = true; break; }
                                cc += step;
                            }
                            if !blocked { return true; }
                        } else if c == kc && r != kr {
                            let step = if r < kr { 1i32 } else { -1i32 };
                            let mut rr = r as i32 + step;
                            let mut blocked = false;
                            while rr != kr as i32 {
                                if self.board[rr as usize][c] != 0 { blocked = true; break; }
                                rr += step;
                            }
                            if !blocked { return true; }
                        }
                    }
                    4 | 11 => {
                        let dr = (r as i32 - kr as i32).abs();
                        let dc = (c as i32 - kc as i32).abs();
                        if (dr == 2 && dc == 1) || (dr == 1 && dc == 2) {
                            let (br, bc) = if dr == 2 {
                                ((r as i32 + kr as i32) / 2, c as i32)
                            } else {
                                (r as i32, (c as i32 + kc as i32) / 2)
                            };
                            if in_board(br, bc) && self.board[br as usize][bc as usize] == 0 {
                                return true;
                            }
                        }
                    }
                    6 | 13 => {
                        if r == kr && c != kc {
                            let step = if c < kc { 1i32 } else { -1i32 };
                            let mut count = 0;
                            let mut cc = c as i32 + step;
                            while cc != kc as i32 {
                                if self.board[r][cc as usize] != 0 { count += 1; }
                                cc += step;
                            }
                            if count == 1 { return true; }
                        } else if c == kc && r != kr {
                            let step = if r < kr { 1i32 } else { -1i32 };
                            let mut count = 0;
                            let mut rr = r as i32 + step;
                            while rr != kr as i32 {
                                if self.board[rr as usize][c] != 0 { count += 1; }
                                rr += step;
                            }
                            if count == 1 { return true; }
                        }
                    }
                    7 => {
                        if r as i32 - 1 == kr as i32 && c == kc { return true; }
                        if r == kr && (c as i32 - kc as i32).abs() == 1 && r <= 5 { return true; }
                    }
                    14 => {
                        if r as i32 + 1 == kr as i32 && c == kc { return true; }
                        if r == kr && (c as i32 - kc as i32).abs() == 1 && r >= 4 { return true; }
                    }
                    1 | 8 => {
                        if c == kc {
                            let mut blocked = false;
                            for rr in (std::cmp::min(r, kr) + 1)..std::cmp::max(r, kr) {
                                if self.board[rr][c] != 0 { blocked = true; break; }
                            }
                            if !blocked { return true; }
                        }
                    }
                    _ => {}
                }
            }
        }
        false
    }

    fn generate_piece_moves(&self, r: i32, c: i32) -> Vec<Move> {
        let mut moves = Vec::new();
        let p = self.board[r as usize][c as usize];
        if p == 0 { return moves; }

        let mut add_if_valid = |tr: i32, tc: i32| {
            if !in_board(tr, tc) { return; }
            let tp = self.board[tr as usize][tc as usize];
            if tp != 0 && same_side(p, tp) { return; }
            let mut ns = self.clone();
            ns.board[tr as usize][tc as usize] = p;
            ns.board[r as usize][c as usize] = 0;
            if !ns.is_in_check(is_red(p)) {
                moves.push(Move::new(r, c, tr, tc));
            }
        };

        match p {
            1 | 8 => {
                for dr in -1i32..=1i32 {
                    for dc in -1i32..=1i32 {
                        if dr.abs() + dc.abs() != 1 { continue; }
                        let (tr, tc) = (r + dr, c + dc);
                        if is_red(p) {
                            if tr >= 7 && tr <= 9 && tc >= 3 && tc <= 5 { add_if_valid(tr, tc); }
                        } else {
                            if tr >= 0 && tr <= 2 && tc >= 3 && tc <= 5 { add_if_valid(tr, tc); }
                        }
                    }
                }
            }
            2 | 9 => {
                for dr in -1i32..=1i32 {
                    for dc in -1i32..=1i32 {
                        if dr.abs() != 1 || dc.abs() != 1 { continue; }
                        let (tr, tc) = (r + dr, c + dc);
                        if is_red(p) {
                            if tr >= 7 && tr <= 9 && tc >= 3 && tc <= 5 { add_if_valid(tr, tc); }
                        } else {
                            if tr >= 0 && tr <= 2 && tc >= 3 && tc <= 5 { add_if_valid(tr, tc); }
                        }
                    }
                }
            }
            3 | 10 => {
                for dr in -2i32..=2i32 {
                    for dc in -2i32..=2i32 {
                        if dr.abs() != 2 || dc.abs() != 2 { continue; }
                        let (br, bc) = (r + dr / 2, c + dc / 2);
                        if !in_board(br, bc) || self.board[br as usize][bc as usize] != 0 { continue; }
                        let (tr, tc) = (r + dr, c + dc);
                        if is_red(p) {
                            if tr >= 5 && tr <= 9 && tc >= 0 && tc <= 8 { add_if_valid(tr, tc); }
                        } else {
                            if tr >= 0 && tr <= 4 && tc >= 0 && tc <= 8 { add_if_valid(tr, tc); }
                        }
                    }
                }
            }
            4 | 11 => {
                let offsets = [(-2i32,-1i32),(-2i32,1i32),(-1i32,-2i32),(-1i32,2i32),(1i32,-2i32),(1i32,2i32),(2i32,-1i32),(2i32,1i32)];
                for (dr, dc) in offsets {
                    let (tr, tc) = (r + dr, c + dc);
                    if !in_board(tr, tc) { continue; }
                    let (br, bc) = if dr.abs() == 2 { (r + dr / 2, c) } else { (r, c + dc / 2) };
                    if in_board(br, bc) && self.board[br as usize][bc as usize] == 0 {
                        add_if_valid(tr, tc);
                    }
                }
            }
            5 | 12 => {
                for dr in -1i32..=1i32 {
                    for dc in -1i32..=1i32 {
                        if dr.abs() + dc.abs() != 1 { continue; }
                        let (mut tr, mut tc) = (r + dr, c + dc);
                        while in_board(tr, tc) {
                            let tp = self.board[tr as usize][tc as usize];
                            if tp == 0 { add_if_valid(tr, tc); }
                            else if !same_side(p, tp) { add_if_valid(tr, tc); break; }
                            else { break; }
                            tr += dr; tc += dc;
                        }
                    }
                }
            }
            6 | 13 => {
                for dr in -1i32..=1i32 {
                    for dc in -1i32..=1i32 {
                        if dr.abs() + dc.abs() != 1 { continue; }
                        let (mut tr, mut tc) = (r + dr, c + dc);
                        while in_board(tr, tc) && self.board[tr as usize][tc as usize] == 0 {
                            add_if_valid(tr, tc);
                            tr += dr; tc += dc;
                        }
                        tr += dr; tc += dc;
                        while in_board(tr, tc) {
                            let tp = self.board[tr as usize][tc as usize];
                            if tp != 0 {
                                if !same_side(p, tp) { add_if_valid(tr, tc); }
                                break;
                            }
                            tr += dr; tc += dc;
                        }
                    }
                }
            }
            7 => {
                add_if_valid(r - 1, c);
                if r <= 5 { add_if_valid(r, c - 1); add_if_valid(r, c + 1); }
            }
            14 => {
                add_if_valid(r + 1, c);
                if r >= 4 { add_if_valid(r, c - 1); add_if_valid(r, c + 1); }
            }
            _ => {}
        }
        moves
    }

    fn generate_all_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        for r in 0..10 {
            for c in 0..9 {
                let p = self.board[r][c];
                if p == 0 { continue; }
                if self.red_turn && !is_red(p) { continue; }
                if !self.red_turn && !is_black(p) { continue; }
                moves.extend(self.generate_piece_moves(r as i32, c as i32));
            }
        }
        moves
    }

    fn make_move(&mut self, m: Move) {
        self.captured.push(self.board[m.to_r as usize][m.to_c as usize]);
        self.board[m.to_r as usize][m.to_c as usize] = self.board[m.from_r as usize][m.from_c as usize];
        self.board[m.from_r as usize][m.from_c as usize] = 0;
        self.red_turn = !self.red_turn;
        self.history.push(m);
    }

    /*fn undo_move(&mut self) {
        if let Some(m) = self.history.pop() {
            if let Some(cap) = self.captured.pop() {
                self.board[m.from_r as usize][m.from_c as usize] = self.board[m.to_r as usize][m.to_c as usize];
                self.board[m.to_r as usize][m.to_c as usize] = cap;
                self.red_turn = !self.red_turn;
            }
        }
    }*/
}

// ============================================================
// 评估函数
// ============================================================

const PIECE_VALUE: [i32; 15] = [
    0, 100000, 250, 250, 500, 1000, 500, 120,
    100000, 250, 250, 500, 1000, 500, 120
];

const PAWN_POS_R: [[i32; 9]; 10] = [
    [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0],
    [70,70,70,80,90,80,70,70,70], [80,80,90,100,110,100,90,80,80],
    [90,90,100,110,120,110,100,90,90], [100,100,110,120,130,120,110,100,100],
    [110,110,120,130,140,130,120,110,110],
];

const KNIGHT_POS: [[i32; 9]; 10] = [
    [20,30,40,40,40,40,40,30,20], [30,50,60,70,70,70,60,50,30],
    [40,60,80,90,90,90,80,60,40], [40,70,90,100,100,100,90,70,40],
    [40,70,90,100,110,100,90,70,40], [40,70,90,100,110,100,90,70,40],
    [40,70,90,100,100,100,90,70,40], [40,60,80,90,90,90,80,60,40],
    [30,50,60,70,70,70,60,50,30], [20,30,40,40,40,40,40,30,20],
];

const ROOK_POS: [[i32; 9]; 10] = [
    [100,100,100,100,100,100,100,100,100], [100,110,110,110,110,110,110,110,100],
    [100,110,120,120,120,120,120,110,100], [100,110,120,130,130,130,120,110,100],
    [100,110,120,130,140,130,120,110,100], [100,110,120,130,140,130,120,110,100],
    [100,110,120,130,130,130,120,110,100], [100,110,120,120,120,120,120,110,100],
    [100,110,110,110,110,110,110,110,100], [100,100,100,100,100,100,100,100,100],
];

const CANNON_POS: [[i32; 9]; 10] = [
    [60,60,60,60,60,60,60,60,60], [60,70,70,70,70,70,70,70,60],
    [60,70,80,80,80,80,80,70,60], [60,70,80,90,90,90,80,70,60],
    [60,70,80,90,100,90,80,70,60], [60,70,80,90,100,90,80,70,60],
    [60,70,80,90,90,90,80,70,60], [60,70,80,80,80,80,80,70,60],
    [60,70,70,70,70,70,70,70,60], [60,60,60,60,60,60,60,60,60],
];

fn evaluate(b: &Board) -> i32 {
    let mut score = 0i32;
    for r in 0..10 {
        for c in 0..9 {
            let p = b.board[r][c];
            if p == 0 { continue; }
            let mut val = PIECE_VALUE[p as usize];
            match p {
                7 => val += PAWN_POS_R[r][c],
                14 => val += PAWN_POS_R[9-r][c],
                4 => val += KNIGHT_POS[r][c],
                11 => val += KNIGHT_POS[9-r][c],
                5 => val += ROOK_POS[r][c],
                12 => val += ROOK_POS[9-r][c],
                6 => val += CANNON_POS[r][c],
                13 => val += CANNON_POS[9-r][c],
                _ => {}
            }
            if is_red(p) { score += val; } else { score -= val; }
        }
    }
    let mobility = b.generate_all_moves().len() as i32 * 3;
    if b.red_turn { score += mobility; } else { score -= mobility; }
    if !b.red_turn { score = -score; }
    score
}

// ============================================================
// Alpha-Beta 搜索
// ============================================================

struct Searcher {
    nodes: u64,
    time_limit: Duration,
    start_time: Instant,
    history: [[[i32; 9]; 10]; 2],
    max_depth: i32,
    stop_flag: Option<Arc<AtomicBool>>,
    uci_mode: bool,
}

impl Searcher {
    fn new(time_limit_secs: f64) -> Self {
        Searcher {
            nodes: 0,
            time_limit: Duration::from_secs_f64(time_limit_secs),
            start_time: Instant::now(),
            history: [[[0; 9]; 10]; 2],
            max_depth: 6,
            stop_flag: None,
            uci_mode: false,
        }
    }

    fn set_max_depth(&mut self, depth: i32) {
        self.max_depth = depth;
    }

    fn set_stop_flag(&mut self, flag: Arc<AtomicBool>) {
        self.stop_flag = Some(flag);
    }

    fn set_uci_mode(&mut self, mode: bool) {
        self.uci_mode = mode;
    }

    fn timeout(&self) -> bool {
        if self.start_time.elapsed() > self.time_limit {
            return true;
        }
        if let Some(ref flag) = self.stop_flag {
            if flag.load(Ordering::Relaxed) {
                return true;
            }
        }
        false
    }

    fn quiescence(&mut self, b: &Board, mut alpha: i32, beta: i32, depth: i32) -> i32 {
        self.nodes += 1;
        if self.timeout() { return evaluate(b); }
        let stand_pat = evaluate(b);
        if stand_pat >= beta { return beta; }
        if alpha < stand_pat { alpha = stand_pat; }
        if alpha >= beta { return beta; }
        if depth <= -4 { return stand_pat; }

        let moves = b.generate_all_moves();
        let mut captures: Vec<Move> = moves.into_iter()
            .filter(|m| b.board[m.to_r as usize][m.to_c as usize] != 0)
            .collect();

        captures.sort_by(|a, b_move| {
            let va = PIECE_VALUE[b.board[b_move.to_r as usize][b_move.to_c as usize] as usize];
            let vb = PIECE_VALUE[b.board[a.to_r as usize][a.to_c as usize] as usize];
            va.cmp(&vb)
        });

        for m in captures {
            let mut ns = b.clone();
            ns.make_move(m);
            let score = -self.quiescence(&ns, -beta, -alpha, depth - 1);
            if score >= beta { return beta; }
            if score > alpha { alpha = score; }
            if alpha >= beta { return beta; }
        }
        alpha
    }

    fn alpha_beta(&mut self, b: &Board, depth: i32, mut alpha: i32, beta: i32) -> i32 {
        self.nodes += 1;
        if self.timeout() { return evaluate(b); }
        if depth <= 0 { return self.quiescence(b, alpha, beta, 0); }

        let moves = b.generate_all_moves();
        if moves.is_empty() {
            if b.is_in_check(b.red_turn) { return -30000 + depth * 100; }
            else { return 0; }
        }

        let side_idx = if b.red_turn { 0 } else { 1 };
        let mut sorted_moves = moves;
        sorted_moves.sort_by(|a, b_move| {
            let ha = self.history[side_idx][b_move.to_r as usize][b_move.to_c as usize];
            let hb = self.history[side_idx][a.to_r as usize][a.to_c as usize];
            hb.cmp(&ha)
        });

        for m in sorted_moves {
            let mut ns = b.clone();
            ns.make_move(m);
            let score = -self.alpha_beta(&ns, depth - 1, -beta, -alpha);
            if score >= beta {
                let side_idx = if b.red_turn { 0 } else { 1 };
                self.history[side_idx][m.to_r as usize][m.to_c as usize] += depth * depth;
                return beta;
            }
            if score > alpha { alpha = score; }
        }
        alpha
    }

    fn find_best_move(&mut self, b: &Board) -> Option<Move> {
        self.nodes = 0;
        self.start_time = Instant::now();
        let moves = b.generate_all_moves();
        if moves.is_empty() { return None; }

        let mut best_move = moves[0];
        let mut best_score = -999999;

        for depth in 1..=self.max_depth {
            if self.timeout() && depth > 1 { break; }
            let mut current_best = moves[0];
            let mut current_score = -999999;

            for (i, m) in moves.iter().enumerate() {
                if self.timeout() && depth > 1 { break; }
                let mut ns = b.clone();
                ns.make_move(*m);
                let score = -self.alpha_beta(&ns, depth - 1, -99999, 99999);
                if score > current_score {
                    current_score = score;
                    current_best = *m;
                }
                if self.uci_mode {
                    let elapsed = self.start_time.elapsed().as_millis() as u64;
                    let nps = if elapsed > 0 { self.nodes * 1000 / elapsed as u64 } else { 0 };
                    println!("info depth {} currmove {} currmovenumber {} nodes {} time {} nps {}",
                        depth, move_to_uci(m), i + 1, self.nodes, elapsed, nps);
                } else if i == 0 && depth >= 3 {
                    print!("\r  深度 {} 评分: {} 节点: {}     ", depth, current_score, self.nodes);
                    let _ = io::stdout().flush();
                }
            }
            if !self.timeout() || depth == 1 {
                best_move = current_best;
                best_score = current_score;
            }
            if self.uci_mode {
                let elapsed = self.start_time.elapsed().as_millis() as u64;
                let nps = if elapsed > 0 { self.nodes * 1000 / elapsed as u64 } else { 0 };
                println!("info depth {} score cp {} nodes {} time {} nps {} pv {}",
                    depth, best_score, self.nodes, elapsed, nps, move_to_uci(&best_move));
            }
        }
        if !self.uci_mode {
            print!("\r                                      \r");
            let _ = io::stdout().flush();
        }
        Some(best_move)
    }
}


// ============================================================
// UCI 引擎通信 (与 Pikafish 等外部引擎对弈)
// ============================================================

struct UciEngine {
    process: Child,
    stdin: ChildStdin,
    reader: BufReader<ChildStdout>,
}

impl UciEngine {
    fn new(path: &str) -> Result<Self, String> {
        let mut child = Command::new(path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("无法启动引擎 {}: {}", path, e))?;

        let stdin = child.stdin.take().ok_or("无法获取 stdin")?;
        let stdout = child.stdout.take().ok_or("无法获取 stdout")?;
        let reader = BufReader::new(stdout);

        let mut engine = UciEngine { process: child, stdin, reader };
        engine.send("uci");
        engine.wait_for("uciok")?;
        engine.send("isready");
        engine.wait_for("readyok")?;
        Ok(engine)
    }

    fn send(&mut self, cmd: &str) {
        let _ = writeln!(self.stdin, "{}", cmd);
        let _ = self.stdin.flush();
    }

    fn read_line(&mut self) -> Result<String, String> {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(0) => Err("引擎已关闭".to_string()),
            Ok(_) => Ok(line.trim().to_string()),
            Err(e) => Err(format!("读取错误: {}", e)),
        }
    }

    fn wait_for(&mut self, keyword: &str) -> Result<Vec<String>, String> {
        let mut lines = Vec::new();
        loop {
            let line = self.read_line()?;
            if !line.is_empty() {
                lines.push(line.clone());
            }
            if line.contains(keyword) {
                break;
            }
        }
        Ok(lines)
    }

    fn get_best_move(&mut self, fen_or_position: &str, movetime_ms: u64) -> Result<String, String> {
        self.send("ucinewgame");
        self.send(fen_or_position);
        self.send(&format!("go movetime {}", movetime_ms));

        let lines = self.wait_for("bestmove")?;
        for line in &lines {
            if line.starts_with("bestmove") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    return Ok(parts[1].to_string());
                }
            }
        }
        Err("未收到 bestmove".to_string())
    }

    fn quit(&mut self) {
        let _ = self.send("quit");
        let _ = self.process.wait();
    }
}

// ============================================================
// UCI 协议支持
// ============================================================

fn move_to_uci(m: &Move) -> String {
    // Pikafish UCI: row 0 = 红方底线 (内部 board[9]), row 9 = 黑方底线 (内部 board[0])
    format!("{}{}{}{}",
        (b'a' + m.from_c as u8) as char,
        (b'0' + (9 - m.from_r) as u8) as char,
        (b'a' + m.to_c as u8) as char,
        (b'0' + (9 - m.to_r) as u8) as char)
}

fn parse_position(board: &mut Board, line: &str) {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let mut idx = 0;

    if idx < parts.len() && parts[idx] == "position" {
        idx += 1;
    }

    if idx >= parts.len() { return; }

    if parts[idx] == "startpos" {
        *board = Board::new();
        idx += 1;
        if idx < parts.len() && parts[idx] == "moves" {
            for i in (idx + 1)..parts.len() {
                if let Some(m) = board.uci_move_to_move(parts[i]) {
                    board.make_move(m);
                }
            }
        }
    } else if parts[idx] == "fen" {
        idx += 1;
        let mut fen_parts = Vec::new();
        while idx < parts.len() && parts[idx] != "moves" {
            fen_parts.push(parts[idx]);
            idx += 1;
        }
        let fen = fen_parts.join(" ");
        if let Ok(b) = Board::from_fen(&fen) {
            *board = b;
        }
        if idx < parts.len() && parts[idx] == "moves" {
            for i in (idx + 1)..parts.len() {
                if let Some(m) = board.uci_move_to_move(parts[i]) {
                    board.make_move(m);
                }
            }
        }
    }
}

fn parse_go(parts: &[&str], red_turn: bool) -> (Option<i32>, Option<u64>, bool, Option<i32>) {
    let mut depth: Option<i32> = None;
    let mut movetime: Option<u64> = None;
    let mut infinite: bool = false;
    let mut perft_depth: Option<i32> = None;
    let mut wtime: Option<u64> = None;
    let mut btime: Option<u64> = None;
    let mut winc = 0u64;
    let mut binc = 0u64;
    let mut movestogo: Option<u64> = None;

    let mut i = 1;
    while i < parts.len() {
        match parts[i] {
            "depth" => { i += 1; if i < parts.len() { depth = parts[i].parse().ok(); } }
            "movetime" => { i += 1; if i < parts.len() { movetime = parts[i].parse().ok(); } }
            "infinite" => { infinite = true; }
            "wtime" => { i += 1; if i < parts.len() { wtime = parts[i].parse().ok(); } }
            "btime" => { i += 1; if i < parts.len() { btime = parts[i].parse().ok(); } }
            "winc" => { i += 1; if i < parts.len() { winc = parts[i].parse().unwrap_or(0); } }
            "binc" => { i += 1; if i < parts.len() { binc = parts[i].parse().unwrap_or(0); } }
            "movestogo" => { i += 1; if i < parts.len() { movestogo = parts[i].parse().ok(); } }
            "perft" => { i += 1; if i < parts.len() { perft_depth = parts[i].parse().ok(); } }
            _ => {}
        }
        i += 1;
    }

    // 根据wtime/btime自动分配时间
    if movetime.is_none() && !infinite && depth.is_none() && perft_depth.is_none() {
        let time = if red_turn { wtime } else { btime };
        let inc = if red_turn { winc } else { binc };
        if let Some(t) = time {
            let mut allocated = t / 30 + inc;
            if let Some(mtg) = movestogo {
                if mtg > 0 {
                    allocated = t / mtg as u64 + inc;
                }
            }
            allocated = std::cmp::min(allocated, t / 2);
            if allocated < 100 { allocated = 100; }
            movetime = Some(allocated);
        }
    }

    (depth, movetime, infinite, perft_depth)
}

fn perft(b: &Board, depth: i32) -> u64 {
    if depth <= 0 { return 1; }
    let moves = b.generate_all_moves();
    if depth == 1 { return moves.len() as u64; }
    let mut count = 0u64;
    for m in moves {
        let mut nb = b.clone();
        nb.make_move(m);
        count += perft(&nb, depth - 1);
    }
    count
}

fn uci_loop() {
    let mut board = Board::new();
    let stdin = io::stdin();
    let mut stop_flag: Option<Arc<AtomicBool>> = None;
    let mut search_handle: Option<thread::JoinHandle<()>> = None;

    loop {
        let mut line = String::new();
        if stdin.read_line(&mut line).is_err() { break; }
        let line = line.trim();
        if line.is_empty() { continue; }

        let parts: Vec<&str> = line.split_whitespace().collect();
        let cmd = parts[0];

        match cmd {
            "uci" => {
                println!("id name RustXiangqi v2.3");
                println!("id author TermuxUser");
                println!("option name Hash type spin default 1 min 1 max 256");
                println!("option name Threads type spin default 1 min 1 max 1");
                println!("option name Ponder type check default false");
                println!("uciok");
            }
            "isready" => {
                println!("readyok");
            }
            "ucinewgame" => {
                board = Board::new();
            }
            "position" | "startpos" | "fen" => {
                parse_position(&mut board, line);
            }
            "go" => {
                let (depth, movetime, infinite, perft_depth) = parse_go(&parts, board.red_turn);

                // 停止之前的搜索
                if let Some(ref flag) = stop_flag {
                    flag.store(true, Ordering::Relaxed);
                }
                if let Some(handle) = search_handle.take() {
                    let _ = handle.join();
                }

                // perft 直接计算，不需要线程
                if let Some(pd) = perft_depth {
                    let nodes = perft(&board, pd);
                    println!("info nodes {}", nodes);
                    println!("bestmove 0000");
                    continue;
                }

                let flag = Arc::new(AtomicBool::new(false));
                stop_flag = Some(flag.clone());
                let board_clone = board.clone();

                search_handle = Some(thread::spawn(move || {
                    let mut time_limit = 5.0;
                    if let Some(ms) = movetime {
                        time_limit = ms as f64 / 1000.0;
                    }
                    let mut searcher = Searcher::new(time_limit);
                    searcher.set_uci_mode(true);
                    if let Some(d) = depth {
                        searcher.set_max_depth(d);
                    }
                    searcher.set_stop_flag(flag);

                    if infinite {
                        searcher.set_max_depth(99);
                        searcher.time_limit = Duration::from_secs(99999);
                    }

                    if let Some(best) = searcher.find_best_move(&board_clone) {
                        let uci_move = move_to_uci(&best);
                        println!("bestmove {}", uci_move);
                    } else {
                        println!("bestmove 0000");
                    }
                }));
            }
            "stop" => {
                if let Some(ref flag) = stop_flag {
                    flag.store(true, Ordering::Relaxed);
                }
                if let Some(handle) = search_handle.take() {
                    let _ = handle.join();
                }
            }
            "quit" => {
                if let Some(ref flag) = stop_flag {
                    flag.store(true, Ordering::Relaxed);
                }
                if let Some(handle) = search_handle.take() {
                    let _ = handle.join();
                }
                break;
            }
            "d" => {
                print_board(&board, -1, -1, &[]);
            }
            "eval" => {
                let score = evaluate(&board);
                println!("info score cp {}", score);
            }
            "setoption" => {
                // 忽略所有setoption命令
            }
            _ => {}
        }
    }
}

// ============================================================
// 终端UI
// ============================================================

fn print_board(b: &Board, sel_r: i32, sel_c: i32, targets: &[(i32, i32)]) {
    print!("[2J[H");
    let _ = io::stdout().flush();

    println!();
    println!("{}", yellow("      ╔══════════ 中国象棋 ══════════╗"));
    println!();
    println!("         0    1    2    3    4    5    6    7    8");
    println!("       +----+----+----+----+----+----+----+----+----+");

    for r in 0..10 {
        print!("     {} │", r);
        for c in 0..9 {
            let p = b.board[r][c];
            let is_sel = r as i32 == sel_r && c as i32 == sel_c;
            let is_tgt = targets.contains(&(r as i32, c as i32));

            let name = if p == 0 { "  " } else { PIECE_NAMES[p as usize] };

            let mut cell = name.to_string();
            if is_sel {
                cell = bg_select(&cell);
            } else if is_tgt {
                cell = bg_target(&cell);
            }

            if p != 0 {
                cell = if is_red(p) { red(&cell) } else { blue(&cell) };
            }
            print!(" {} │", cell);
        }
        println!();
        if r < 9 {
            println!("       +----+----+----+----+----+----+----+----+----+");
        }
    }
    println!("       +----+----+----+----+----+----+----+----+----+");
    println!();
    if b.red_turn {
        println!("{}", green("      ► 轮到: 红方 (帅)"));
    } else {
        println!("{}", blue("      ► 轮到: 黑方 (将)"));
    }
    println!();
    let _ = io::stdout().flush();
}

fn coord_input(prompt: &str) -> (i32, i32) {
    loop {
        print!("{}", prompt);
        let _ = io::stdout().flush();
        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() { continue; }
        let input = input.trim();
        if input.len() >= 2 {
            if let (Ok(r), Ok(c)) = (input[0..1].parse::<i32>(), input[1..2].parse::<i32>()) {
                if in_board(r, c) { return (r, c); }
            }
        }
        println!("    格式: 两位数字如 73 表示第7行第3列");
    }
}

fn read_line() -> String {
    let mut s = String::new();
    let _ = io::stdin().read_line(&mut s);
    s.trim().to_string()
}

// ============================================================
// 主程序
// ============================================================

fn terminal_mode() {
    let mut board = Board::new();

    println!();
    println!("{}", yellow("  ╔═══════════════════════════════════════════╗"));
    println!("{}", yellow("  ║         中国象棋 - Rust引擎 v2.3          ║"));
    println!("{}", yellow("  ╠═══════════════════════════════════════════╣"));
    println!("{}", yellow("  ║  1. 人机对弈 (玩家执红先走)               ║"));
    println!("{}", yellow("  ║  2. 人机对弈 (玩家执黑后走)               ║"));
    println!("{}", yellow("  ║  3. AI自战演示                            ║"));
    println!("{}", yellow("  ║  4. 人人对弈                              ║"));
    println!("{}", yellow("  ║  5. 与 Pikafish 对弈 (UCI外部引擎)        ║"));
    println!("{}", yellow("  ╚═══════════════════════════════════════════╝"));
    println!();
    print!("  选择模式 (1-5): ");
    let _ = io::stdout().flush();

    let mode_str = read_line();
    let mode: i32 = mode_str.parse().unwrap_or(1);
    let mode = if mode < 1 || mode > 5 { 1 } else { mode };

    let ai_delay = if mode == 3 { 500 } else { 0 };

    // Pikafish 引擎设置
    let mut pikafish_engine: Option<UciEngine> = None;
    let mut pikafish_time = 3000u64;
    let mut player_is_red = true;

    if mode == 5 {
        println!();
        print!("  输入 Pikafish 引擎路径 (如 ./pikafish): ");
        let _ = io::stdout().flush();
        let path = read_line();
        //let pikafish_path = if path.is_empty() { "./pikafish" } else { &path };
        let pikafish_path = if path.is_empty() { "/data/data/com.termux/files/home/bin/pikafish"} else { &path };

        print!("  Pikafish 每步思考时间(ms) [默认3000]: ");
        let _ = io::stdout().flush();
        let time_str = read_line();
        if let Ok(t) = time_str.parse::<u64>() {
            if t > 0 { pikafish_time = t; }
        }

        print!("  玩家执红先走? (y/n) [默认y]: ");
        let _ = io::stdout().flush();
        let color_str = read_line().to_lowercase();
        player_is_red = color_str.is_empty() || color_str.starts_with('y');

        println!();
        println!("  正在启动 Pikafish...");
        match UciEngine::new(pikafish_path) {
            Ok(engine) => {
                pikafish_engine = Some(engine);
                println!("  ✓ Pikafish 已连接！");
                println!("  玩家执{}, Pikafish 执{}",
                    if player_is_red { "红" } else { "黑" },
                    if player_is_red { "黑" } else { "红" });
            }
            Err(e) => {
                println!("  ✗ 无法启动 Pikafish: {}", e);
                println!("  请检查路径是否正确，按 Enter 退出...");
                let _ = read_line();
                return;
            }
        }
        println!();
    }

    loop {
        print_board(&board, -1, -1, &[]);

        let moves = board.generate_all_moves();
        if moves.is_empty() {
            if board.is_in_check(board.red_turn) {
                if board.red_turn {
                    println!("{}", blue("    ═══════════════════════════════════════"));
                    println!("{}", blue("         黑方胜利！将死红帅！"));
                    println!("{}", blue("    ═══════════════════════════════════════"));
                } else {
                    println!("{}", red("    ═══════════════════════════════════════"));
                    println!("{}", red("         红方胜利！将死黑将！"));
                    println!("{}", red("    ═══════════════════════════════════════"));
                }
            } else {
                println!("{}", yellow("    ═══════════════════════════════════════"));
                println!("{}", yellow("              和棋！无子可动"));
                println!("{}", yellow("    ═══════════════════════════════════════"));
            }
            println!();
            println!("  按 Enter 退出...");
            let _ = read_line();
            break;
        }

        let is_player = mode == 4
            || (mode == 1 && board.red_turn)
            || (mode == 2 && !board.red_turn)
            || (mode == 5 && board.red_turn == player_is_red);

        if is_player {
            let mut valid = false;
            while !valid {
                let (fr, fc) = coord_input("  选择棋子 (行列如73): ");
                let p = board.board[fr as usize][fc as usize];
                if p == 0 {
                    println!("  ⚠ 此处无棋子！");
                    continue;
                }
                if board.red_turn && !is_red(p) {
                    println!("  ⚠ 请选红方棋子！");
                    continue;
                }
                if !board.red_turn && !is_black(p) {
                    println!("  ⚠ 请选黑方棋子！");
                    continue;
                }

                let piece_moves = board.generate_piece_moves(fr, fc);
                if piece_moves.is_empty() {
                    println!("  ⚠ 此棋子无合法走法！");
                    continue;
                }

                let targets: Vec<(i32, i32)> = piece_moves.iter().map(|m| (m.to_r, m.to_c)).collect();
                print_board(&board, fr, fc, &targets);

                println!("  可走位置:");
                for (i, m) in piece_moves.iter().enumerate() {
                    print!("    {}: ({},{})", i + 1, m.to_r, m.to_c);
                    if board.board[m.to_r as usize][m.to_c as usize] != 0 {
                        print!(" [吃{}]", PIECE_NAMES[board.board[m.to_r as usize][m.to_c as usize] as usize]);
                    }
                    if (i + 1) % 2 == 0 { println!(); }
                }
                if piece_moves.len() % 2 != 0 { println!(); }
                println!();

                let (tr, tc) = coord_input("  目标位置 (行列如64): ");
                let mut found = false;
                for m in &piece_moves {
                    if m.to_r == tr && m.to_c == tc {
                        let cap = board.board[tr as usize][tc as usize];
                        board.make_move(*m);
                        print_board(&board, -1, -1, &[]);
                        if cap != 0 {
                            println!("  ✓ 吃掉 {}!", PIECE_NAMES[cap as usize]);
                        }
                        found = true;
                        valid = true;
                        break;
                    }
                }
                if !found {
                    println!("  ✗ 非法走法！");
                }
            }
        } else if mode == 5 {
            // Pikafish 引擎走子
            println!("  Pikafish 思考中...");
            if let Some(ref mut engine) = pikafish_engine {
                let fen = board.to_fen();
                let pos_cmd = format!("position fen {}", fen);
                match engine.get_best_move(&pos_cmd, pikafish_time) {
                    Ok(uci_move) => {
                        if let Some(m) = board.uci_move_to_move(&uci_move) {
                            let p = board.board[m.from_r as usize][m.from_c as usize];
                            let t = board.board[m.to_r as usize][m.to_c as usize];
                            board.make_move(m);
                            print_board(&board, -1, -1, &[]);
                            print!("  Pikafish: {} ", uci_move);
                            if p != 0 {
                                print!("({})", PIECE_NAMES[p as usize]);
                            }
                            if t != 0 {
                                print!(" 吃{}", PIECE_NAMES[t as usize]);
                            }
                            println!();
                        } else {
                            println!("  ✗ Pikafish 返回非法走法: {}", uci_move);
                            break;
                        }
                    }
                    Err(e) => {
                        println!("  ✗ Pikafish 通信错误: {}", e);
                        break;
                    }
                }
            } else {
                println!("  ✗ Pikafish 引擎未连接！");
                break;
            }
        } else {
            println!("  AI思考中...");
            let mut searcher = Searcher::new(5.0);
            if let Some(ai_move) = searcher.find_best_move(&board) {
                let p = board.board[ai_move.from_r as usize][ai_move.from_c as usize];
                let t = board.board[ai_move.to_r as usize][ai_move.to_c as usize];
                board.make_move(ai_move);
                print_board(&board, -1, -1, &[]);
                print!("  AI: ({},{}) → ({},{}) {}",
                    ai_move.from_r, ai_move.from_c, ai_move.to_r, ai_move.to_c,
                    PIECE_NAMES[p as usize]);
                if t != 0 {
                    print!(" 吃{}", PIECE_NAMES[t as usize]);
                }
                println!();
            } else {
                println!("  AI无合法走法！");
                break;
            }
        }

        if mode == 3 {
            std::thread::sleep(Duration::from_millis(ai_delay));
        }
    }

    // 清理 Pikafish 引擎
    if let Some(mut engine) = pikafish_engine {
        engine.quit();
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // 命令行参数 --uci 或 uci 进入UCI模式
    if args.len() > 1 && (args[1] == "--uci" || args[1] == "-u" || args[1] == "uci") {
        uci_loop();
        return;
    }

    // 默认进入终端交互模式
    terminal_mode();
}
