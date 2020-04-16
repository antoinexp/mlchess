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

(** game.ml *)

let checkBounds a b = (a>=0 && b>=0 && a<8 && b<8)


type move_direction = {
	_from:Board.position;
	_to:Board.position;
}

type move =
	  Promote of move_direction*Board.piece (* piece to promote *)
	| Castle of move_direction*move_direction (* king,rook move *)
	| En_passant of move_direction
	| Move of move_direction


let pawn_dir board (i,j) color0 =
	let color0 = Some color0 in
	let forwardMove (i1,j1) =
		if Board.((get board (i1,j1)) = Empty) 
		then (
			let movedir = {_from=(i,j); _to=(i1,j1)} in
			if i1=0 || i1=7 then (
				List.map 
					(fun p -> Promote (movedir,p)) 
					Board.all_strong_pieces
			) else [Move movedir]
		) else []
	in 
	let sideMove (i1,j1) =
		if Board.(
			(two_opposite_colors (color (get board (i1,j1))) color0)
			&& (two_opposite_colors (color (get board (i,j1))) color0)
		) then [En_passant {_from=(i,j); _to=(i1,j1)}]
		else []
	in let startMove (i0,j0) (i1,j1) =
		if Board.( ((get board (i1,j1)) = Empty)
			&& ((get board (i0,j0)) = Empty)
		) then [Move {_from=(i,j); _to=(i1,j1)}]
		else []
	in let listDirs dir = 
		let i1 = i+dir in
		if i1=7 || i1=0 then [] else
			List.concat [
				(if j<7 then sideMove (i1,j+1) else []);
				(if j>0 then sideMove (i1,j-1) else []);
				(if i=1 && dir>0 then startMove (i1,j) (i1+dir,j) else []);
				(if i=6 && dir<0 then startMove (i1,j) (i1+dir,j) else []);
				(forwardMove (i1,j))
			]
	in match color0 with 
	  Some Board.White -> listDirs 1 
	| Some Board.Black -> listDirs (-1)
	| _ -> []
	


let fold_dir board (i,j) color0 rolling dir =
	let rec f rolling (x,y) l (dx,dy) =
	 	let x1,y1 = x+dx, y+dy in
	 	if not (checkBounds x1 y1) then l
	 	else Board.(
	 		let p = get board (x1,y1) in
	 		let movedir = Move {_from=(i,j); _to=(x1,y1)} in
	 		if p<>Empty then (
	 			if (color p) = Some color0 then l
	 			else movedir::l
	 		) 
	 		else if rolling then movedir::(f rolling (x1,y1) l (dx,dy))
	 		else movedir::l
	 	)
	in
	Array.fold_left (f rolling (i,j)) [] dir

let rookDir = [| (1,0); (-1,0); (0,1); (0,-1) |]
let bishopDir = [| (1,1); (1,-1); (-1,-1); (-1,1) |]
let queenDir = Array.append rookDir bishopDir

let kingDir = queenDir
let knightDir = [|  (-2,1); (-1,2); (1,2); (2,1); 
					(2,-1); (1,-2); (-1,-2); (-2,-1); |]

let piece_dir (board:Board.t) p color1 =
	Board.(
		let f = fold_dir board p color1 in
		let moveList = 
		function
			  Knight -> f false knightDir
			| King -> f false kingDir
			| Queen -> f true queenDir
			| Bishop -> f true bishopDir
			| Rook -> f true rookDir
			| Pawn -> pawn_dir board p color1
		in 
		match get board p with
		  Empty -> []
		| Figure (color0, _) when color0 <> color1 -> []
		| Figure (_, piece) -> moveList piece
	)


let list_move (board:Board.t) color1 =
	let f i j = piece_dir board (i,j) color1 in
	let t = Array.init 8 (fun i -> Array.init 8 (f i)) in
	Array.fold_left (Array.fold_left List.append) [] t


let print_move =
	let num_to_alpha i = "abcdefgh".[i] in
	let f m = 
		Printf.printf "%c%d-%c%d\n" 
		(num_to_alpha (snd m._from)) (1+(fst m._from))
		(num_to_alpha (snd m._to)) (1+(fst m._to))
	in function 
	  Move m -> f m
	| Castle (m,_) -> f m
	| En_passant m -> f m
	| Promote (m,_) -> f m


let apply_move (board:Board.t) move =
	Board.(
		let board' = copy board in
		let f m =
			let p = get board' m._from in
			set board' m._from Empty;
			set board' m._to p;
			set_last_move board' (m._from, m._to)
		in 
		(match move with
		  Move m -> f m
		| Castle (m,_) -> f m
		| En_passant m -> f m
		| Promote (m,_) -> f m
		); board'
	)


