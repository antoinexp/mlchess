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

(** main.ml *)

let get_random_move board color =
	let l = Game.list_move board color in
    List.nth l (Random.int (List.length l))

let next_color = 
	function 
	  Board.Black -> Board.White 
	| Board.White -> Board.Black 

let _ =
	let board = Board.initialBoard () in
	Board.print board; 
	(*let l = Game.list_move board Board.White in
	List.iter Game.print_move l;*)
	let rec f board color =
		function
		  0 -> ()
		| n -> (
			let move = get_random_move board color in
			let board_new = Game.apply_move board move in
			Board.print board_new;
			f board_new (next_color color) (n-1)
		  )
	in f board Board.Black 150
