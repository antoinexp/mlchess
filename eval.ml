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

(** eval.ml *)


let piece_value =
	Board.(
		function
		  Pawn -> 1
		| Bishop -> 3
		| Knight -> 3
		| Rook -> 5
		| Queen -> 9
		| King -> 100
	)

type base_feature = {
	mutable color: Board.color option;
	mutable piece_worth: int;
	mutable kill_potential: int;
	mutable death_potential: int;
	mutable move_potential: int;
}

let new_base_feature () =
	{	
		color=None;
		piece_worth=0;
		kill_potential=0;
		death_potential=0;
		move_potential=0;
	}


let base_feature_map (board:Board.t) =
	let f i j = 
		Board.get_and_apply board (i,j) None
		(fun color piece -> 
			Some (color, piece, Game.piece_dir board (i,j) color)
		) 
	in
	let t = Array.init 8 (fun i -> Array.init 8 (f i)) in
	let feature_map = Array.init 8 (fun i -> Array.init 8 
		(fun j -> new_base_feature ())
	) in
	let compute_feature (i,j) color piece moves =
		let feature = feature_map.(i).(j) in
		feature.color<-Some color;
		feature.piece_worth<-piece_value piece;
		feature.move_potential<-List.length moves;

		let move_potential m =
			Board.get_and_apply board (i,j) () (
				fun _ piece' -> 
				feature.kill_potential<-feature.kill_potential+(piece_value piece');
				feature.move_potential<-feature.move_potential+1;
				let feature' = Game.(feature_map.(fst m._to).(snd m._to)) in
				feature'.death_potential<-feature'.death_potential+(piece_value piece')
			)
		in List.iter Game.(
			function
			  Move m -> move_potential m
			| Castle (m,_) -> move_potential m (* todo *)
			| En_passant m -> move_potential m (* todo *)
			| Promote (m,_) -> move_potential m (* todo *)
		) moves
	in
	for i=0 to 7 do
		for j=0 to 7 do
			match t.(i).(j) with
				  Some (color, piece, moves) -> 
				  	compute_feature (i,j) color piece moves
				| None -> ()
		done
	done;
	feature_map


let aggregate_base_feature_map feature_map color =
	let result = {(new_base_feature ()) with color=Some color} in
	Array.fold_left (Array.fold_left (fun result -> 
		function
		    feature when feature.color = Some color ->
		    {result with 
		    	piece_worth=result.piece_worth + feature.piece_worth;
		    	kill_potential=result.kill_potential + feature.kill_potential;
		    	death_potential=result.death_potential + feature.death_potential;
				move_potential=result.move_potential + feature.move_potential
		    }
		  | _ -> result
	)) result feature_map

let base_features (board:Board.t) =
	let last_color = Game.get_last_color board in
	let next_color = Game.next_color last_color in
	let feature_map = base_feature_map board in
	let b_ft =	aggregate_base_feature_map feature_map Board.Black in
	let w_ft =	aggregate_base_feature_map feature_map Board.White in
	[|
		if next_color==Board.White then 1. else -1.;
		float (w_ft.piece_worth - b_ft.piece_worth);
		float (w_ft.kill_potential - b_ft.kill_potential);
		float (w_ft.death_potential - b_ft.death_potential);
		float (w_ft.move_potential - b_ft.move_potential);
	|]


let sigmoid a x = 
	2.*.(
		(1. 
			/. 
			(1.+.exp (-.x /. a))
		) -. 0.5)


let score_board (board:Board.t) =
	let base_ft = base_features board in
	let score0 = Array.fold_left ( +. ) 0. [|
		base_ft.(0);
		(sigmoid 1.0 base_ft.(1));
		sigmoid 1.0 base_ft.(2);
		sigmoid 1.0 base_ft.(3);
		sigmoid 1.0 base_ft.(4);
	|]
	in  sigmoid 1.0 score0















