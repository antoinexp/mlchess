(***************************************************************************
 *   Copyright (C) 2016  Antoine Bodin <antoinexp@gmail.com>               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or         *
 *   modify it under the terms of the GNU General Public License           *
 *   as published by the Free Software Foundation; either version 2        * 
 *   of the License, or (at your option) any later version.                *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA          *
 ***************************************************************************)

(** board.ml *)

(** 280 bits - 35 bytes:
	- 4*64 bits: pieces on board (EMPTY | (color)*(p|r|n|b|q|k))
	- 2*8 bits: coordinates last moved piece ((1-8)*(a-h))*2
	- 8 bits: ((CASTLE_LEFT_RIGHT | CASTLE_LEFT | CASTLE_RIGHT | NO_CASTLE)*(COLOR))
*)
type t = bytes

(** [make ()] set up a new empty chessboard *)
let make () = Bytes.init 35 (fun _ -> '\000')

let copy = Bytes.copy


type castling = 
	  Castle_both
	| Castle_left
	| Castle_right
	| Castle_none


type piece = 
	  Pawn 
	| Bishop 
	| Knight 
	| Rook 
	| King 
	| Queen

let all_strong_pieces = [Bishop; Knight; Rook; King; Queen]

type color = Black | White

type castling_side = color*castling

type figure = Empty | Figure of color*piece

let color = 
	function 
		  Figure (c,_) -> Some c
		| Empty -> None

let two_opposite_colors a b =
	match a,b with
	  (Some Black), (Some White) -> true
	| (Some White), (Some Black) -> true
	| _ -> false

let string_of_color =
	function
	  Some White -> "white"
	| Some Black -> "black"
	| None -> ""

type position = int*int

exception Wrong_value

let piece_of_int = 
	function
	  0 -> Pawn
	| 1 -> Bishop
	| 2 -> Knight
	| 3 -> Rook
	| 4 -> Queen
	| 5 -> King
	| _ -> raise Wrong_value

let int_of_piece =
	function
	  Pawn -> 0
	| Bishop -> 1
	| Knight -> 2
	| Rook -> 3
	| Queen -> 4
	| King -> 5

let color_of_int =
	function
	  0 -> Black
	| 1 -> White
	| _ -> raise Wrong_value

let int_of_color = 
	function
	  Black -> 0
	| White -> 1

let figure_of_int figure =
	try (
		Figure (color_of_int (figure land 1), piece_of_int (figure lsr 1))
	) with Wrong_value -> Empty

let int_of_figure =
	function 
	  Empty -> 0b1111
	| Figure (color, piece) ->
		(lor) 
		((int_of_color color)) 
		((int_of_piece piece) lsl 1)

let get (chessboard:t) (i,j) = 
	let q = (8*i+j) lsr 1 in
	let case = int_of_char (Bytes.get chessboard q) in
	let figure = 
		if (j land 1)==1 then (case lsr 4)
		else (case land 0b1111)
	in figure_of_int figure

let set (chessboard:t) (i,j) figure =
	let q = (8*i+j) lsr 1 in
	let case = int_of_char (Bytes.get chessboard q) in
	let figure_int = int_of_figure figure in
	let new_case = 
	  	if (j land 1) == 1 then 
	  		(case land (0b1111)) lor (figure_int lsl 4)
		else ((case lsr 4) lsl 4) lor figure_int
    in 
    Bytes.set chessboard q (char_of_int new_case)


let get_last_move (chessboard:t) =
	let a = int_of_char (Bytes.get chessboard 33) in
	let b = int_of_char (Bytes.get chessboard 34) in
	((
		((a lsr 0) land 0b1111),
		((a lsr 4) land 0b1111)
	), 
	(
		((b lsr 0) land 0b1111),
		((b lsr 4) land 0b1111)
	))


let set_last_move (chessboard:t) ((i,j),(k,l)) =
	let a = ((j lsl 4) lor i) in
	let b = ((l lsl 4) lor k) in
	Bytes.set chessboard 33 (char_of_int a);
	Bytes.set chessboard 34 (char_of_int b)


let get_castling (chessboard:t) =
	let b = int_of_char (Bytes.get chessboard 35) in
	let f = 
	function
		  0 -> Castle_both
		| 1 -> Castle_right
		| 2 -> Castle_left
		| 3 -> Castle_both
		| _ -> raise Wrong_value
	in
	(White, f (b land 0b1111)), (Black, f (b lsr 4))


let set_castling (chessboard:t) ((color,castle):castling_side) =
	let b = int_of_char (Bytes.get chessboard 35) in
	let f = 
	function
		  Castle_both -> 0
		| Castle_right -> 1
		| Castle_left -> 2
		| Castle_none -> 3
	in let g  =
	function
		  White -> (fun x -> (x lsl 4) lor (b land 0b1111))
		| Black -> (lor) ((b lsr 4) lsl 4)
	in
	let b_new = char_of_int ((g color) (f castle)) in
	Bytes.set chessboard 35 b_new


(** [init f] creates a chessboard with pieces [f i j] *)
let init (f:position->figure) =
	let board = make () in
	for i=0 to 7 do
      for j=0 to 7 do
      	set board (i,j) (f (i,j))
  	  done
	done;
	board


(** [initialBoard ()] creates a default chessboard *)
let initialBoard () = 
	let f = 
	  	function
		  (1,_) 		-> Figure (White, Pawn)
	    | (6,_) 		-> Figure (Black, Pawn)
	    | (0,0) | (0,7) -> Figure (White, Rook)
	    | (7,0) | (7,7) -> Figure (Black, Rook)
	    | (0,1) | (0,6) -> Figure (White, Knight)
	    | (7,1) | (7,6) -> Figure (Black, Knight)
	    | (0,2) | (0,5) -> Figure (White, Bishop)
	    | (7,2) | (7,5) -> Figure (Black, Bishop)
	    | (0,3) 		-> Figure (White, Queen)
	    | (7,3) 		-> Figure (Black, Queen)
	    | (0,4) 		-> Figure (White, King)
	    | (7,4) 		-> Figure (Black, King)
	    | _ 			-> Empty
	in init f


(** []  *)
let char_of_figure =
	let g = 
	function
	  | Pawn   -> 'p'
	  | Rook   -> 'r'
	  | Knight -> 'n'
	  | Bishop -> 'b'
	  | Queen  -> 'q'
	  | King   -> 'k'
	in let f = 
	function
	  | Figure (Black, piece) -> Char.uppercase_ascii (g piece)
	  | Figure (White, piece) -> (g piece)
	  | Empty -> '.' 
	in f

let print (chb:t) =
	for i=7 downto 0 do
      for j=0 to 7 do
    	let piece = get chb (i,j) in
    	Printf.printf "%c" (char_of_figure piece)
      done;
      print_newline ()
	done;
    let a,b = get_last_move chb in
    Printf.printf "last piece moved: (%d,%d) (%d,%d)\n"
  		(fst a) (snd a) (fst b) (snd b)





