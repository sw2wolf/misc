%%% xq_uci.erl
%%% Xiangqi (Chinese chess) + UCI engine frontend.
%%% Faithful port of xq_uci.lisp
%%%
%%% Build:
%%%   erlc xq_uci.erl
%%% Run:
%%%   erl -noshell -eval 'xq_uci:main(), halt().'
-module(xq_uci).
-export([main/0]).
%% -*- coding: utf-8 -*-
%% ========================================================
%% Piece constants
%% ========================================================
-define(EMPTY,     0).
-define(R_KING,    1).
-define(R_ADVISOR, 2).
-define(R_BISHOP,  3).
-define(R_KNIGHT,  4).
-define(R_ROOK,    5).
-define(R_CANNON,  6).
-define(R_PAWN,    7).
-define(B_KING,    8).
-define(B_ADVISOR, 9).
-define(B_BISHOP,  10).
-define(B_KNIGHT,  11).
-define(B_ROOK,    12).
-define(B_CANNON,  13).
-define(B_PAWN,    14).
%% ========================================================
%% Piece names (UTF-8 binaries)
%% ========================================================
piece_name(?EMPTY)     -> <<"  ">>;
piece_name(?R_KING)    -> <<"帅"/utf8>>;
piece_name(?R_ADVISOR) -> <<"仕"/utf8>>;
piece_name(?R_BISHOP)  -> <<"相"/utf8>>;
piece_name(?R_KNIGHT)  -> <<"马"/utf8>>;
piece_name(?R_ROOK)    -> <<"车"/utf8>>;
piece_name(?R_CANNON)  -> <<"炮"/utf8>>;
piece_name(?R_PAWN)    -> <<"兵"/utf8>>;
piece_name(?B_KING)    -> <<"将"/utf8>>;
piece_name(?B_ADVISOR) -> <<"士"/utf8>>;
piece_name(?B_BISHOP)  -> <<"象"/utf8>>;
piece_name(?B_KNIGHT)  -> <<"马"/utf8>>;
piece_name(?B_ROOK)    -> <<"车"/utf8>>;
piece_name(?B_CANNON)  -> <<"炮"/utf8>>;
piece_name(?B_PAWN)    -> <<"卒"/utf8>>.
%% ========================================================
%% Basic predicates
%% ========================================================
is_red(P)   -> P >= 1 andalso P =< 7.
is_black(P) -> P >= 8 andalso P =< 14.
in_board(R, C) -> R >= 0 andalso R =< 9 andalso C >= 0 andalso C =< 8.

same_side(A, B) ->
  (is_red(A)  andalso is_red(B))
      orelse (is_black(A) andalso is_black(B)).

in_palace(true,  R, C) -> R >= 0 andalso R =< 2 andalso C >= 3 andalso C =< 5;
in_palace(false, R, C) -> R >= 7 andalso R =< 9 andalso C >= 3 andalso C =< 5.
%% ========================================================
%% Board accessors
%% Board = #{cells => {Row0..Row9}, red_turn => bool(), captured => [int]}
%% Row = tuple of 9 ints
%% ========================================================
piece_at(#{cells := Cells}, R, C) ->
  element(C + 1, element(R + 1, Cells)).

set_piece(#{cells := Cells} = B, R, C, P) ->
  Row    = element(R + 1, Cells),
  NewRow = setelement(C + 1, Row, P),
  B#{cells := setelement(R + 1, Cells, NewRow)}.

new_board() ->
  Empty = list_to_tuple(lists:duplicate(9, 0)),
  Rows  = list_to_tuple(lists:duplicate(10, Empty)),
  B0    = #{cells => Rows, red_turn => true, captured => []},
  lists:foldl(fun({R, C, P}, B) -> set_piece(B, R, C, P) end, B0,
    [{9,0,?B_ROOK},   {9,1,?B_KNIGHT}, {9,2,?B_BISHOP},
     {9,3,?B_ADVISOR},{9,4,?B_KING},   {9,5,?B_ADVISOR},
     {9,6,?B_BISHOP}, {9,7,?B_KNIGHT}, {9,8,?B_ROOK},
     {7,1,?B_CANNON}, {7,7,?B_CANNON},
     {6,0,?B_PAWN},   {6,2,?B_PAWN},   {6,4,?B_PAWN},
     {6,6,?B_PAWN},   {6,8,?B_PAWN},
     {3,0,?R_PAWN},   {3,2,?R_PAWN},   {3,4,?R_PAWN},
     {3,6,?R_PAWN},   {3,8,?R_PAWN},
     {2,1,?R_CANNON}, {2,7,?R_CANNON},
     {0,0,?R_ROOK},   {0,1,?R_KNIGHT}, {0,2,?R_BISHOP},
     {0,3,?R_ADVISOR},{0,4,?R_KING},   {0,5,?R_ADVISOR},
     {0,6,?R_BISHOP}, {0,7,?R_KNIGHT}, {0,8,?R_ROOK}]).
%% ========================================================
%% FEN
%% ========================================================
piece_char(12) -> $r;
piece_char(11) -> $n;
piece_char(10) -> $b;
piece_char(9)  -> $a;
piece_char(8)  -> $k;
piece_char(13) -> $c;
piece_char(14) -> $p;
piece_char(5)  -> $R;
piece_char(4)  -> $N;
piece_char(3)  -> $B;
piece_char(2)  -> $A;
piece_char(1)  -> $K;
piece_char(6)  -> $C;
piece_char(7)  -> $P;
piece_char(_)  -> $?.

to_fen(Board) ->
  RowsStr = [fen_row(Board, R) || R <- lists:seq(9, 0, -1)],
  Body    = string:join(RowsStr, "/"),
  Turn    = case maps:get(red_turn, Board) of
    true  -> " w";
    false -> " b"
  end,
  Body ++ Turn ++ " - - 0 1".

fen_row(Board, R) -> fen_row(Board, R, 0, 0, []).
fen_row(_Board, _R, 9, 0, Acc) ->
  string:join(lists:reverse(Acc), "");
fen_row(_Board, _R, 9, Empty, Acc) ->
  string:join(lists:reverse([integer_to_list(Empty) | Acc]), "");
fen_row(Board, R, C, Empty, Acc) ->
  P = piece_at(Board, R, C),
  case P of
    0 -> fen_row(Board, R, C + 1, Empty + 1, Acc);
    _ ->
      NewAcc = case Empty of
        0 -> [[piece_char(P)] | Acc];
        _ -> [[piece_char(P)], integer_to_list(Empty) | Acc]
      end,
      fen_row(Board, R, C + 1, 0, NewAcc)
  end.
%% ========================================================
%% King lookup and check detection
%% ========================================================
find_king(Board, RedSide) ->
  Target = case RedSide of
    true  -> ?R_KING;
    false -> ?B_KING
  end,
  find_piece(Board, Target, 0, 0).

find_piece(_Board, _T, 10, _) -> not_found;
find_piece(Board, T, R, 9)   -> find_piece(Board, T, R + 1, 0);
find_piece(Board, T, R, C) ->
  case piece_at(Board, R, C) of
    T -> {R, C};
    _ -> find_piece(Board, T, R, C + 1)
  end.

clear_h(Board, R, C1, C2) ->
  Lo = min(C1, C2) + 1,
  Hi = max(C1, C2) - 1,
  lists:all(fun(C) -> piece_at(Board, R, C) =:= 0 end, lists:seq(Lo, Hi)).

clear_v(Board, C, R1, R2) ->
  Lo = min(R1, R2) + 1,
  Hi = max(R1, R2) - 1,
  lists:all(fun(R) -> piece_at(Board, R, C) =:= 0 end, lists:seq(Lo, Hi)).

rook_attacks_p(Board, R, C, KR, KC) ->
  (R =:= KR andalso C =/= KC andalso clear_h(Board, R, C, KC)) orelse (C =:= KC andalso R =/= KR andalso clear_v(Board, C, R, KR)).

knight_attacks_p(Board, R, C, KR, KC) ->
  DR = abs(R - KR),
  DC = abs(C - KC),
  case (DR =:= 2 andalso DC =:= 1) orelse (DR =:= 1 andalso DC =:= 2) of
    false -> false;
    true ->
      {BR, BC} = case DR of
        2 -> {(R + KR) div 2, C};
        1 -> {R, (C + KC) div 2}
      end,
      in_board(BR, BC) andalso piece_at(Board, BR, BC) =:= 0
  end.

cannon_attacks_p(Board, R, C, KR, KC) ->
  if
    R =:= KR, C =/= KC ->
      Count = length([1 || CC <- lists:seq(min(C, KC) + 1, max(C, KC) - 1),piece_at(Board, R, CC) =/= 0]),
      Count =:= 1;
    C =:= KC, R =/= KR ->
      Count = length([1 || RR <- lists:seq(min(R, KR) + 1, max(R, KR) - 1), piece_at(Board, RR, C) =/= 0]),
      Count =:= 1;
    true -> false
  end.

red_pawn_attacks_p(R, C, KR, KC) ->
  (R + 1 =:= KR andalso C =:= KC) orelse
  (R =:= KR andalso abs(C - KC) =:= 1 andalso R >= 5).

black_pawn_attacks_p(R, C, KR, KC) ->
  (R - 1 =:= KR andalso C =:= KC) orelse 
  (R =:= KR andalso abs(C - KC) =:= 1 andalso R =< 4).

king_attacks_p(Board, R, C, KR, KC) ->
  C =:= KC andalso clear_v(Board, C, R, KR).

attacks_p(B, R, C, P, KR, KC) when P =:= ?R_ROOK; P =:= ?B_ROOK ->
  rook_attacks_p(B, R, C, KR, KC);
attacks_p(B, R, C, P, KR, KC) when P =:= ?R_KNIGHT; P =:= ?B_KNIGHT ->
  knight_attacks_p(B, R, C, KR, KC);
attacks_p(B, R, C, P, KR, KC) when P =:= ?R_CANNON; P =:= ?B_CANNON ->
  cannon_attacks_p(B, R, C, KR, KC);
attacks_p(_, R, C, ?R_PAWN, KR, KC) ->
  red_pawn_attacks_p(R, C, KR, KC);
attacks_p(_, R, C, ?B_PAWN, KR, KC) ->
  black_pawn_attacks_p(R, C, KR, KC);
attacks_p(B, R, C, P, KR, KC) when P =:= ?R_KING; P =:= ?B_KING ->
  king_attacks_p(B, R, C, KR, KC);
attacks_p(_, _, _, _, _, _) -> false.

is_in_check(Board, RedSide) ->
  case find_king(Board, RedSide) of
    not_found -> false;
    {KR, KC} ->
      lists:any(
        fun(R) ->
          lists:any(
            fun(C) ->
              P = piece_at(Board, R, C),
              Enemy = case RedSide of
                true  -> is_black(P);
                false -> is_red(P)
              end,
              Enemy andalso attacks_p(Board, R, C, P, KR, KC)
            end, lists:seq(0, 8))
        end, lists:seq(0, 9))
  end.
%% ========================================================
%% Move generation
%% ========================================================
add_if_valid(Board, Piece, R, C, TR, TC, Moves) ->
  case in_board(TR, TC) of
    false -> Moves;
    true ->
      TP = piece_at(Board, TR, TC),
      case same_side(Piece, TP) of
        true -> Moves;
        false ->
          Test = set_piece(set_piece(Board, TR, TC, Piece), R, C, ?EMPTY),
          case is_in_check(Test, is_red(Piece)) of
            true  -> Moves;
            false -> [{R, C, TR, TC} | Moves]
          end
      end
  end.

king_moves(B, P, R, C) ->
  Red = is_red(P),
  lists:foldl(
    fun({DR, DC}, Acc) ->
      TR = R + DR, TC = C + DC,
      case in_palace(Red, TR, TC) of
        true  -> add_if_valid(B, P, R, C, TR, TC, Acc);
        false -> Acc
      end
    end, [], [{-1,0},{1,0},{0,-1},{0,1}]).

advisor_moves(B, P, R, C) ->
  Red = is_red(P),
  lists:foldl(
    fun({DR, DC}, Acc) ->
      TR = R + DR, TC = C + DC,
      case in_palace(Red, TR, TC) of
        true  -> add_if_valid(B, P, R, C, TR, TC, Acc);
        false -> Acc
      end
    end, [], [{-1,-1},{-1,1},{1,-1},{1,1}]).

bishop_moves(B, P, R, C) ->
  Red = is_red(P),
  lists:foldl(
    fun({DR, DC}, Acc) ->
      BR = R + DR div 2, BC = C + DC div 2,
      TR = R + DR, TC = C + DC,
      case in_board(BR, BC) andalso piece_at(B, BR, BC) =:= 0 of
        false -> Acc;
        true ->
          Ok = case Red of
            true  -> TR >= 0 andalso TR =< 4;
            false -> TR >= 5 andalso TR =< 9
          end,
          case Ok of
            true  -> add_if_valid(B, P, R, C, TR, TC, Acc);
            false -> Acc
          end
      end
    end, [], [{-2,-2},{-2,2},{2,-2},{2,2}]).

knight_moves(B, P, R, C) ->
  Deltas = [{-2,-1},{-2,1},{-1,-2},{-1,2},{1,-2},{1,2},{2,-1},{2,1}],
  lists:foldl(
    fun({DR, DC}, Acc) ->
      TR = R + DR, TC = C + DC,
      case in_board(TR, TC) of
        false -> Acc;
        true ->
          {BR, BC} = case abs(DR) of
            2 -> {R + DR div 2, C};
            1 -> {R, C + DC div 2}
          end,
          case piece_at(B, BR, BC) =:= 0 of
            true  -> add_if_valid(B, P, R, C, TR, TC, Acc);
            false -> Acc
          end
      end
  end, [], Deltas).

rook_moves(B, P, R, C) ->
  Acc0 = [],
  Acc1 = slide_rook(B, P, R, C, -1, 0, Acc0),
  Acc2 = slide_rook(B, P, R, C,  1, 0, Acc1),
  Acc3 = slide_rook(B, P, R, C,  0, -1, Acc2),
  slide_rook(B, P, R, C, 0, 1, Acc3).

slide_rook(B, P, R, C, DR, DC, Acc) ->
    TR0 = R + DR,
    TC0 = C + DC,
    slide_rook(B, P, R, C, TR0, TC0, DR, DC, Acc).

slide_rook(_B, _P, _R, _C, TR, TC, _DR, _DC, Acc)
  when TR < 0; TR > 9; TC < 0; TC > 8 -> Acc;
slide_rook(B, P, R, C, TR, TC, DR, DC, Acc) ->
  case piece_at(B, TR, TC) of
    0 -> slide_rook(B, P, R, C, TR + DR, TC + DC, DR, DC,
add_if_valid(B, P, R, C, TR, TC, Acc));
    TP ->
      case same_side(P, TP) of
        true  -> Acc;
        false -> add_if_valid(B, P, R, C, TR, TC, Acc)
      end
  end.

cannon_moves(B, P, R, C) ->
  Acc0 = [],
  Acc1 = slide_cannon(B, P, R, C, -1, 0, Acc0),
  Acc2 = slide_cannon(B, P, R, C,  1, 0, Acc1),
  Acc3 = slide_cannon(B, P, R, C,  0, -1, Acc2),
  slide_cannon(B, P, R, C, 0, 1, Acc3).

slide_cannon(B, P, R, C, DR, DC, Acc0) ->
  {Acc1, TR1, TC1} = cannon_empties(B, P, R, C, R + DR, C + DC, DR, DC, Acc0),
  case in_board(TR1, TC1) of
    false -> Acc1;
    true ->
      {TR3, TC3} = cannon_skip(B, TR1 + DR, TC1 + DC, DR, DC),
      case in_board(TR3, TC3) of
        false -> Acc1;
        true ->
          case same_side(P, piece_at(B, TR3, TC3)) of
            true  -> Acc1;
            false -> add_if_valid(B, P, R, C, TR3, TC3, Acc1)
          end
      end
  end.

cannon_empties(_B, _P, _R, _C, TR, TC, _DR, _DC, Acc)
  when TR < 0; TR > 9; TC < 0; TC > 8 -> {Acc, TR, TC};
cannon_empties(B, P, R, C, TR, TC, DR, DC, Acc) ->
  case piece_at(B, TR, TC) of
    0 -> cannon_empties(B, P, R, C, TR + DR, TC + DC, DR, DC, add_if_valid(B, P, R, C, TR, TC, Acc));
    _ -> {Acc, TR, TC}
  end.

cannon_skip(_B, TR, TC, _DR, _DC) when TR < 0; TR > 9; TC < 0; TC > 8 -> {TR, TC};
cannon_skip(B, TR, TC, DR, DC) ->
  case piece_at(B, TR, TC) of
    0 -> cannon_skip(B, TR + DR, TC + DC, DR, DC);
    _ -> {TR, TC}
  end.

pawn_moves(B, P, R, C) ->
  case is_red(P) of
    true ->
      Acc1 = add_if_valid(B, P, R, C, R + 1, C, []),
      case R >= 5 of
        true ->
          Acc2 = add_if_valid(B, P, R, C, R, C - 1, Acc1),
          add_if_valid(B, P, R, C, R, C + 1, Acc2);
        false -> Acc1
      end;
    false ->
      Acc1 = add_if_valid(B, P, R, C, R - 1, C, []),
      case R =< 4 of
        true ->
          Acc2 = add_if_valid(B, P, R, C, R, C - 1, Acc1),
          add_if_valid(B, P, R, C, R, C + 1, Acc2);
        false -> Acc1
      end
  end.

generate_piece_moves(B, R, C) ->
  case piece_at(B, R, C) of
    0 -> [];
    P when P =:= ?R_KING;    P =:= ?B_KING    -> king_moves(B, P, R, C);
    P when P =:= ?R_ADVISOR; P =:= ?B_ADVISOR -> advisor_moves(B, P, R, C);
    P when P =:= ?R_BISHOP;  P =:= ?B_BISHOP  -> bishop_moves(B, P, R, C);
    P when P =:= ?R_KNIGHT;  P =:= ?B_KNIGHT  -> knight_moves(B, P, R, C);
    P when P =:= ?R_ROOK;    P =:= ?B_ROOK    -> rook_moves(B, P, R, C);
    P when P =:= ?R_CANNON;  P =:= ?B_CANNON  -> cannon_moves(B, P, R, C);
    P when P =:= ?R_PAWN;    P =:= ?B_PAWN    -> pawn_moves(B, P, R, C);
    _ -> []
  end.

generate_all_moves(Board) ->
  TurnRed = maps:get(red_turn, Board),
  lists:foldl(
    fun(R, AccR) ->
      lists:foldl(
        fun(C, AccC) ->
          P = piece_at(Board, R, C),
          Owner = case TurnRed of
            true  -> is_red(P);
            false -> is_black(P)
          end,
          case P =/= 0 andalso Owner of
            true  -> generate_piece_moves(Board, R, C) ++ AccC;
            false -> AccC
          end
        end, AccR, lists:seq(0, 8))
      end, [], lists:seq(0, 9)).
%% ========================================================
%% Apply move
%% ========================================================
apply_move(#{red_turn := RT, captured := Capt} = B, {FR, FC, TR, TC}) ->
  P    = piece_at(B, FR, FC),
  CapP = piece_at(B, TR, TC),
  B1   = set_piece(B, TR, TC, P),
  B2   = set_piece(B1, FR, FC, ?EMPTY),
  B2#{red_turn := not RT, captured := [CapP | Capt]}.
%% ========================================================
%% UCI move string <-> move tuple
%% ========================================================
uci_move_to_move(Board, [FC, FR, TC, TR | _]) when FC >= $a, FC =< $i, FR >= $0, FR =< $9, TC >= $a, TC =< $i, TR >= $0, TR =< $9 ->
  FromC = FC - $a, FromR = FR - $0,
  ToC   = TC - $a, ToR   = TR - $0,
  case in_board(FromR, FromC) andalso in_board(ToR, ToC) of
    true  -> find_move(generate_piece_moves(Board, FromR, FromC), ToR, ToC);
    false -> not_found
  end;
uci_move_to_move(_Board, _) -> not_found.

find_move(Moves, TR, TC) ->
  case lists:search(
    fun({_, _, TR1, TC1}) -> TR1 =:= TR andalso TC1 =:= TC end, Moves) of
    {value, M} -> M;
    false      -> not_found
  end.
%% ========================================================
%% UCI engine (open_port-based)
%% ========================================================
open_engine(Path) ->
  try
    Port = open_port({spawn_executable, Path},
    [binary, {line, 8192}, exit_status, stderr_to_stdout]),
    
    send_cmd(Port, <<"uci">>),
    case wait_for_token(Port, <<"uciok">>) of
      true ->
        send_cmd(Port, <<"isready">>),
        case wait_for_token(Port, <<"readyok">>) of
          true  -> {ok, Port};
          false ->
            try port_close(Port) catch _:_ -> ok end,
            {error, "missing readyok"}
        end;
      false ->
        try port_close(Port) catch _:_ -> ok end,
        {error, "missing uciok"}
      end
  catch
    Class:Reason ->
      {error, io_lib:format("cannot start engine (~p:~p)", [Class, Reason])}
  end.

send_cmd(Port, Cmd) ->
  port_command(Port, [Cmd, <<"\n">>]).

wait_for_token(Port, Token) ->
  receive
    {Port, {data, {eol, Line}}} ->
      case binary:match(Line, Token) of
        nomatch -> wait_for_token(Port, Token);
        _       -> true
      end;
    {Port, {data, {noeol, _}}} ->
      wait_for_token(Port, Token);
    {Port, {exit_status, _}} -> false;
    {Port, closed} -> false
  after 30000 ->
    false
  end.

get_best_move(Port, PosCmd, MovetimeMs) ->
  send_cmd(Port, <<"ucinewgame">>),
  send_cmd(Port, iolist_to_binary(PosCmd)),
  send_cmd(Port, iolist_to_binary(io_lib:format("go movetime ~b", [MovetimeMs]))),
  best_move_loop(Port).

best_move_loop(Port) ->
  receive
    {Port, {data, {eol, Line}}} ->
      case Line of
        <<"bestmove ", Rest/binary>> ->
          case binary:split(Rest, <<" ">>) of
            [Best | _] -> {ok, binary_to_list(Best)};
            []         -> {error, "no bestmove arg"}
          end;
        _ ->
          best_move_loop(Port)
      end;
    {Port, {data, {noeol, _}}} ->
      best_move_loop(Port);
    {Port, {exit_status, _}} ->
      {error, "EOF while waiting for bestmove"};
    {Port, closed} ->
      {error, "port closed"}
  after 60000 ->
    {error, "timeout"}
  end.

quit_engine(Port) ->
  try send_cmd(Port, <<"quit">>) catch _:_ -> ok end,
  try port_close(Port) catch _:_ -> ok end,
  ok.
%% ========================================================
%% Terminal rendering
%% ========================================================
color_red(S)    -> [27, "[1;31m", S, 27, "[0m"].
color_blue(S)   -> [27, "[1;34m", S, 27, "[0m"].
color_yellow(S) -> [27, "[1;33m", S, 27, "[0m"].
color_green(S)  -> [27, "[1;32m", S, 27, "[0m"].
bg_select(S)    -> [27, "[44;1;37m", S, 27, "[0m"].
bg_target(S)    -> [27, "[42;1;30m", S, 27, "[0m"].

print_board(Board, SelR, SelC, Targets, EngineMoveStr) ->
  io:put_chars([27, "[2J", 27, "[H"]),
  io:format("~n"),
  lists:foreach(fun(R) -> print_row(Board, R, SelR, SelC, Targets) end, lists:seq(9, 0, -1)),
  io:format("        "),
  lists:foreach(fun(C) -> io:format("~w    ", [C]) end, lists:seq(0, 8)), io:format("~n~n"),

  io:format("引擎: ~ts~n", [EngineMoveStr]),
  case maps:get(red_turn, Board) of
    true  -> io:put_chars(color_green(<<"      ► 轮到: 红方 (帅)"/utf8>>));
    false -> io:put_chars(color_blue (<<"      ► 轮到: 黑方 (将)"/utf8>>))
  end,
  io:format("~n~n"),
  ok.

print_row(Board, R, SelR, SelC, Targets) ->
  io:format("      ~w ", [R]),
  lists:foreach(
    fun(C) ->
      print_cell(Board, R, C, SelR, SelC, Targets),
      case C < 8 of
        true ->
          case piece_at(Board, R, C) of
            0 -> io:put_chars(<<"────"/utf8>>);
            _ -> io:put_chars(<<"───"/utf8>>)
          end;
        false -> ok
      end
    end, lists:seq(0, 8)),
  io:format("~n"),
  case R > 0 of
    true  -> print_separator(R);
    false -> ok
  end.

print_cell(Board, R, C, SelR, SelC, Targets) ->
  P    = piece_at(Board, R, C),
  Base = case P of
    0 -> <<"│"/utf8>>;
    _ -> piece_name(P)
  end,
  IsSel = (R =:= SelR) andalso (C =:= SelC),
  IsTgt = lists:member({R, C}, Targets),
  WithBg = case IsSel of
    true  -> bg_select(Base);
    false ->
      case IsTgt of
        true  -> bg_target(Base);
        false -> Base
      end
  end,
  Colored = case P of
    0 -> WithBg;
    _ ->
      case is_red(P) of
        true  -> color_red(WithBg);
        false -> color_blue(WithBg)
      end
  end,
  io:put_chars(Colored).

print_separator(5) ->
  io:format("        "),
  io:put_chars(<<"│        ~~~~~             ~~~~~        │"/utf8>>),
  io:format("~n");
print_separator(R) ->
  io:format("        "),
  lists:foreach(fun(C) -> io:put_chars(sep_cell(C, R)) end, lists:seq(0, 8)),
  io:format("~n").

sep_cell(C, R) ->
  IsDownRight = (C =:= 3 andalso (R =:= 9 orelse R =:= 2))
orelse (C =:= 4 andalso (R =:= 8 orelse R =:= 1)),
  IsDownLeft  = (C =:= 3 andalso (R =:= 8 orelse R =:= 1))
orelse (C =:= 4 andalso (R =:= 9 orelse R =:= 2)),
  case {IsDownRight, IsDownLeft} of
    {true, _} -> <<"│ ╲  "/utf8>>;
    {_, true} -> <<"│ ╱  "/utf8>>;
    _         -> <<"│    "/utf8>>
  end.
%% ========================================================
%% Input
%% ========================================================
strip(Line) -> string:trim(Line, both, " \t\r\n").
read_line() ->
  case io:get_line(standard_io, ">>") of
    eof         -> "";
    {error, _}  -> "";
    Line        -> strip(Line)
  end.

coord_input(Prompt) ->
  io:format("    <q:quit> ~ts~n", [Prompt]),
  case read_line() of
    "q" -> quit;
    [R, C | _] ->
      case {R >= $0 andalso R =< $9, C >= $0 andalso C =< $9} of
        {true, true} ->
          RN = R - $0, CN = C - $0,
          case in_board(RN, CN) of
            true  -> {ok, RN, CN};
            false -> coord_input(Prompt)
          end;
        _ -> coord_input(Prompt)
      end;
    _ -> coord_input(Prompt)
  end.

select_piece(Board) ->
  case coord_input("  选择棋子 (行列如32) ") of
    quit -> quit;
    {ok, FR, FC} ->
      P       = piece_at(Board, FR, FC),
      RedTurn = maps:get(red_turn, Board),
      IsMyPiece = case RedTurn of
        true  -> is_red(P);
        false -> is_black(P)
      end,
      if
        P =:= 0 ->
          io:format("  ⚠ 此处无棋子！~n"),
          select_piece(Board);
        not IsMyPiece ->
          case RedTurn of
            true -> io:format("  ⚠ 请选红方棋子！~n");
            false -> io:format("  ⚠ 请选黑方棋子！~n")
          end,
          select_piece(Board);
        true ->
          case generate_piece_moves(Board, FR, FC) of
            [] ->
              io:format("  ⚠ 此棋子无合法走法！~n"),
              select_piece(Board);
            _ -> {ok, FR, FC}
          end
      end
  end.
%% ========================================================
%% Turn handlers
%% ========================================================
player_turn(Board, EngineMoveStr) ->
  case select_piece(Board) of
    quit -> quit;
    {ok, FR, FC} ->
      PieceMoves = generate_piece_moves(Board, FR, FC),
      Targets    = [{TR, TC} || {_, _, TR, TC} <- PieceMoves],
      print_board(Board, FR, FC, Targets, EngineMoveStr),
      io:format("  可走位置:~n"),
      print_moves(Board, PieceMoves),

      case coord_input("  目标位置 (行列如64) ") of
        quit -> quit;
        {ok, TR, TC} ->
          case find_move(PieceMoves, TR, TC) of
            not_found ->
              io:format("  ✗ 非法走法！~n"),
              player_turn(Board, EngineMoveStr);
            Move ->
              Cap = piece_at(Board, TR, TC),
              NewB = apply_move(Board, Move),
              case Cap of
                0 -> ok;
                _ -> io:format("  ✓ 吃掉 ~ts!~n", [piece_name(Cap)])
              end,
              {ok, NewB}
          end
        end
      end.

print_moves(Board, Moves) ->
  N = lists:foldl(
    fun({_, _, TR, TC}, I) ->
      io:format("    ~w: (~w,~w)", [I, TR, TC]),
      case piece_at(Board, TR, TC) of
        0   -> ok;
        Cap -> io:format(" [吃~ts]", [piece_name(Cap)])
      end,
      case I rem 2 of
        1 -> io:format("~n");
        0 -> ok
      end,
      I + 1
    end, 0, Moves),
  case N rem 2 of
    1 -> io:format("~n");
    0 -> ok
  end,
  io:format("~n").

engine_turn(Board, Port, EngineTime) ->
  io:format("  引擎思考中...~n"),
  FEN     = to_fen(Board),
  PosCmd  = "position fen " ++ FEN,

  case get_best_move(Port, PosCmd, EngineTime) of
    {ok, BestMoveStr} ->
      case uci_move_to_move(Board, BestMoveStr) of
        not_found ->
          io:format("  ✗ 引擎返回非法走法: ~ts~n", [BestMoveStr]),
          error;
        {FR, FC, TR, TC} = Move ->
          NewB    = apply_move(Board, Move),
          MoveStr = io_lib:format("~w,~w->~w,~w", [FR, FC, TR, TC]),
          {ok, NewB, MoveStr}
      end;
    {error, Reason} ->
      io:format("  ✗ 引擎通信错误: ~ts~n", [Reason]),
      error
  end.
%% ========================================================
%% Game loop
%% ========================================================
game_loop(Board, Port, EngineTime, PlayerIsRed, EngineMoveStr) ->
  print_board(Board, -1, -1, [], EngineMoveStr),
  case generate_all_moves(Board) of
  [] ->
    RedTurn = maps:get(red_turn, Board),
    case is_in_check(Board, RedTurn) of
    true ->
      case RedTurn of
      true ->
        io:put_chars(color_blue (<<"═══════════════════════════════════════"/utf8>>)), io:format("~n"),
        io:put_chars(color_blue (<<"         黑方胜利！将死红帅！"/utf8>>)), io:format("~n"),
        io:put_chars(color_blue (<<"═══════════════════════════════════════"/utf8>>)), io:format("~n");
      false ->
        io:put_chars(color_red  (<<"═══════════════════════════════════════"/utf8>>)), io:format("~n"),
        io:put_chars(color_red  (<<"         红方胜利！将死黑将！"/utf8>>)), io:format("~n"),
        io:put_chars(color_red  (<<"═══════════════════════════════════════"/utf8>>)), io:format("~n")
      end;
    false ->
      io:put_chars(color_yellow(<<"═══════════════════════════════════════"/utf8>>)), io:format("~n"),
      io:put_chars(color_yellow(<<"              和棋！无子可动"/utf8>>)), io:format("~n"),
      io:put_chars(color_yellow(<<"═══════════════════════════════════════"/utf8>>)), io:format("~n")
    end,
    io:format("~n按 Enter 退出..."),
    io:get_line(standard_io),
    ok;
  _ ->
    RedTurn = maps:get(red_turn, Board),
    case RedTurn =:= PlayerIsRed of
    true ->
      case player_turn(Board, EngineMoveStr) of
      {ok, NewB} -> game_loop(NewB, Port, EngineTime, PlayerIsRed, EngineMoveStr);
      quit       -> ok
      end;
    false ->
      case engine_turn(Board, Port, EngineTime) of
      {ok, NewB, NewMoveStr} ->
        game_loop(NewB, Port, EngineTime, PlayerIsRed, NewMoveStr);
      error -> ok
      end
  end
end.
%% ========================================================
%% Main
%% ========================================================
main() ->
  %io:setopts(standard_io, [{echo, false}]),
  io:setopts(standard_io, [{encoding, unicode}]),
  Board = new_board(),
  io:format("~n  输入引擎路径 (如 ./pikafish): "),
  Path = case read_line() of
    "" -> "/data/data/com.termux/files/home/bin/pikafish";
    P  -> P
  end,
  io:format("  引擎每步思考时间(ms) [默认9000]: "),
  EngineTime = case read_line() of
    "" -> 9000;
    Ts ->
      case string:to_integer(Ts) of
        {error, _} -> 9000;
        {N, _}     -> N
      end
  end,
  io:format("  玩家执红先走? (y/n) [默认y]: "),
  PlayerIsRed = case string:lowercase(read_line()) of
    "" -> true;
    [C | _] -> C =:= $y
  end,
  io:format("~n  正在启动外部引擎 ...~n"),
  case open_engine(Path) of
    {ok, Port} ->
      try
        io:format("  ✓ 已连接！~n"),
        io:format("  玩家执~ts, 外部引擎执~ts~n~n",
        [case PlayerIsRed of true -> <<"红"/utf8>>; false -> <<"黑"/utf8>> end,
         case PlayerIsRed of true -> <<"黑"/utf8>>; false -> <<"红"/utf8>> end]),
        game_loop(Board, Port, EngineTime, PlayerIsRed, "")
      after
        quit_engine(Port)
      end;
    {error, Reason} ->
      io:format("  ✗ 无法启动外部引擎: ~ts~n", [Reason]),
      io:format("  请检查路径是否正确，按 Enter 退出...~n"),
      io:get_line(standard_io)
  end.
