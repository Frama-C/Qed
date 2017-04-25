(**************************************************************************)
(*                                                                        *)
(*  This file is part of Qed Library                                      *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Trick For Making Sure OcamlDoc is not Blocked                      --- *)
(* -------------------------------------------------------------------------- *)

let read () =
  try
    let inc = open_in ".feedback" in
    let n = int_of_string (input_line inc) in
    close_in inc ; n
  with _ -> 0

let write n =
  try
    let out = open_out ".feedback" in
    output_string out (string_of_int n) ;
    flush out ;
    close_out out ;
  with _ -> ()

let feedback () =
  let s = ref 0 in
  try
    let p = read () in
    while true do
      incr s ;
      let line = read_line () in
      if !s < p then
        Format.printf "\027[K [%2.0f%%] %s\r@?" (100.0 *. float !s /. float p) line
      else
        Format.printf "\027[K [%d] %s\r@?" !s line ;
    done ;
  with _ ->
    write !s ;
    Format.printf
      "\027[KFinished.@."

let trash () =
  try while true do ignore (read_line ()) done with _ -> ()

let () = if Unix.isatty Unix.stdout then feedback () else trash ()

