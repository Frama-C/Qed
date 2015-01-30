(* -------------------------------------------------------------------------- *)
(* --- Proof Assistent                                                    --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Logic
open Ground
open Action
open Grammar
module Proof = Qed.Proof.Make(Ground)
open Proof

(* -------------------------------------------------------------------------- *)
(* --- Sequent                                                            --- *)
(* -------------------------------------------------------------------------- *)

type sequent = {
  local : Ground.env ; (* compiler & pretty printer *)
  context : Proof.context ; (* equalities *)
  definitions : ( string * term ) list ; (* user's context *)
  goal : term ; (* current goal *)
}

let sequent global g = {
  local = global ;
  context = Proof.empty ;
  definitions = [] ;
  goal = g ;
}

(* -------------------------------------------------------------------------- *)
(* --- State                                                              --- *)
(* -------------------------------------------------------------------------- *)

type state = {
  sequent : sequent ; (* current goal with context *)
  script : script ;   (* script mode : corresponds to a state of the
			 parser for the underlying proof grammar *)
} and script =

  (* Initial State: [ - | "proof" ... "qed" ] 
     with initial sequent *)
  | Goal of sequent
      
  (* Final State:   [ "proof" ... "qed" | - ] *)
  | Complete of proof (* Final State *)

  (* Opened Script: [ action... | action... strategy ] 
     The continuation accumulate already parsed actions, 
     and finally receive the strategy. *)
  | Script of state * (proof -> proof)

let goal env prop = 
  let s = sequent env prop in
  { sequent=s ; script=Goal s }
    
(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

let bar k fmt = Format.pp_print_string fmt (String.make k '-')

let pp_script fmt = function
  | _ -> bar 80 fmt
      
let pp_definitions fmt s = 
  begin
    let tab = List.fold_left
      (fun tab (x,_) -> max tab (String.length x)) 0 s.definitions in
    let spaces = String.make tab ' ' in
    List.iter
      (fun (x,v) ->
	 let spc = String.sub spaces (String.length x) tab in
	 if Ground.is_prop v then
	   Format.fprintf fmt "%s%s : @[%a@]@\n" x spc (Ground.pp_env s.local) v
	 else
	   Format.fprintf fmt "%s%s = @[%a@]@\n" x spc (Ground.pp_env s.local) v
      ) s.definitions ;
  end

let pp_goal fmt s =
  Format.fprintf fmt "@[%a]@." (Ground.pp_env s.local) s.goal

let pretty fmt st =
  begin
    pp_definitions fmt st.sequent ;
    pp_script fmt st.script ;
    pp_goal fmt st.sequent ;
  end
	
(* -------------------------------------------------------------------------- *)
(* --- Actions                                                            --- *)
(* -------------------------------------------------------------------------- *)

let check sequent proof =
  try Proof.check sequent.context sequent.goal proof
  with 
    | Proof.Incomplete -> failwith "Proof not complete."
    | Proof.Inconsistent -> failwith "Goal reduces to False."

let rec close_proof proof = function

  | Complete _ ->
      failwith "Proof already complete"

  | Goal s -> 
      check s proof ; (* final check of the entire proof *)
      { sequent=s ; script=Complete proof }

  | Script( stfrom , continuation ) ->
      close_proof (continuation proof) stfrom.script

let close_subproof stfrom proof = function
  | _ -> failwith "No sub-proof to end here"

let apply state action =
  match state.script , action with
(*

    | _ , A_none -> state
    | Goal _ , A_proof ->
	{ state with script = Script( state , fun p -> p ) }

    | Script( { script=Goal _ } , continuation ) , A_qed ->
	close_proof (continuation Proof.Compute) state.script

    | _ , A_qed ->
	(* local check that the proof if finished *)
	check state.sequent Proof.Compute ;
	close_proof Proof.Compute state.script

    | Script( stfrom , continuation ) , A_end ->
	(* local check that the proof if finished *)
	check state.sequent Proof.Compute ;
	(* close_subproof stfrom (continuation Proof.Compute) *) assert false

    | Script( stfrom , continuation ) , A_intro xs ->
	let s,k = introduction state.sequent continuation xs in
	{ sequent = s ; script = Script( stfrom , k ) }
*)
    | _ ->
	failwith "Can not do this in current state"
