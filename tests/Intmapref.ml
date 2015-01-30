(* -------------------------------------------------------------------------- *)
(* --- IntMap Reference                                                   --- *)
(* -------------------------------------------------------------------------- *)

module I = struct type t = int let equal = (=) let compare = Pervasives.compare end
module M = Map.Make(I)
module S = Set.Make(I)

module Spec =
struct

  include M

  let domain m = M.fold (fun k _ d -> S.add k d) m S.empty
  let get k m = try Some(M.find k m) with Not_found -> None

  let union f m1 m2 =
    let d = S.union (domain m1) (domain m2) in
    S.fold
      (fun k m ->
	 let z = match get k m1 , get k m2 with
	   | None , None -> assert false
	   | Some x , Some y -> f k x y
	   | Some x , None -> x
	   | None , Some y -> y
	 in M.add k z m)
      d M.empty
    
  let inter f m1 m2 =
    let d = S.inter (domain m1) (domain m2) in
    S.fold
      (fun k m ->
	 let x = try find k m1 with Not_found -> assert false in
	 let y = try find k m2 with Not_found -> assert false in
	 M.add k (f k x y) m)
      d M.empty

  let interf f m1 m2 =
    let d = S.inter (domain m1) (domain m2) in
    S.fold
      (fun k m ->
	 let x = try find k m1 with Not_found -> assert false in
	 let y = try find k m2 with Not_found -> assert false in
	   match f k x y with
	   | None -> m
	   | Some z -> M.add k z m)
      d M.empty

  let iter2 f m1 m2 =
    let d = S.union (domain m1) (domain m2) in
    S.iter (fun k -> f k (get k m1) (get k m2)) d

  let merge f m1 m2 =
    let d = S.union (domain m1) (domain m2) in
    S.fold
      (fun k m ->
	 match f k (get k m1) (get k m2) with
	   | None -> m
	   | Some z -> M.add k z m)
      d M.empty

  let print msg m =
    Format.printf "%s:" msg ;
    M.iter 
      (fun k d ->
	 Format.printf "@\n + %04X: %S" k d) m ;
    Format.print_newline ()

end

module Data =
struct

  let () = Random.init 0

  let reset () = Random.init 0
  let data () = Printf.sprintf "D%04d" (Random.int 1000)

  let dkeys =
    let dkeys = [| (* base of keys *)
      0b0000000 ;
      0b0000001 ;
      0b0000010 ;
      0b0000100 ;
      0b0010000 ;
      0b0010011 ;
      0b0010110 ;
      0b0010101 ;
      0b0010111 ;
      0b0011000 ;
      0b0011100 ;
      0b0011111 ;
      0b1000000 ;
      0b1001100 ;
      0b1011000 ;
      0b1010100 ;
      0b1011100 ;
      0b1100000 ;
      0b1110000 ;
      0b1111100 ;
      0b1111111 ;
    |]
    in
    let extend_array a f x = (* to extend the base of keys *)
      let n = a.(x lsr 1)
      in if x land 1 = 0 then n else f n
    in
    let dkeys = (* extending dkeys array by replication on the hightest byte *)
      let bytes = match Sys.word_size with
	| 32 -> 4
	| 64 -> 8 
	| _ -> assert false in 
      let shift = bytes-1 in
      let n = Array.length dkeys
      in Array.init (2*n) (extend_array dkeys (fun x -> x lsl shift))
    in
    let dkeys = (* extending dkeys array using one's complement *)
      let n = Array.length dkeys
      in Array.init (2*n) (extend_array dkeys lnot)
    in dkeys

  let shuffle m =
    let n = Array.length m in
    let m = Array.copy m in
    for i=n-1 downto 1 do
      let k = Random.int i in
      let x = m.(k) in
      m.(k) <- m.(i) ;
      m.(i) <- x ;
    done ; m

  let key () = dkeys.( Random.int (Array.length dkeys) )

  let keys () = shuffle dkeys

  let int x = (Random.int (2*x)) - x

end

module type Maping =
sig
  
  type 'a t
  val get : int -> 'a t -> 'a option
  val domain : 'a t -> S.t
  val print : string -> string t -> unit

end

module Oracle(M1 : Maping)(M2 : Maping) =
struct

  let timed = ref None

  let check ?(debug=false) msg m1 m2 =
    begin
      let ok = ref true in
      let warn () =
	if !ok then ( ok := false ; Format.printf "%-12s : FAILED@." msg )
      in
      let domain = S.union (M1.domain m1) (M2.domain m2) in
      S.iter
	(fun k ->
	   match M1.get k m1 , M2.get k m2 with
	     | None , None -> ()
	     | Some x , Some y -> 
		 if x <> y then
		   ( warn () ;
		     if (debug) then Format.printf "DIFF[%04X] spec=%S imap=%S@." k x y )
	     | Some x , None ->
		 warn () ;
		 if (debug) then  Format.printf "MISS[%04X] spec=%S@." k x
	     | None , Some y ->
		 warn () ;
		 if (debug) then Format.printf "MISS[%04X] imap=%S@." k y
	) domain ;
      if !ok then 
	begin
	  Format.printf "%-12s : OK" msg ;
	  match !timed with
	    | None -> Format.print_newline ()
	    | Some (t1,t2) -> Format.printf " time : %6f / %6f@." t1 t2
	end
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Testing Environment                                                --- *)
(* -------------------------------------------------------------------------- *)

module type MergingMap =
sig

  type 'a t
  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val find : int -> 'a t -> 'a
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val mapq : (int -> 'a -> 'a option) -> 'a t -> 'a t
  val iter : (int -> 'a -> unit) -> 'a t -> unit
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val remove : int -> 'a t -> 'a t
  val union : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val interf : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val interq : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val diffq : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val iter2 : (int -> 'a option -> 'b option -> unit) -> 'a t -> 'b t -> unit
  val merge : (int -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val filter : (int -> 'a -> bool) -> 'a t -> 'a t
  val partition : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
    
end

module Monitor(Mmap : MergingMap) =
struct
  type e = { spec : string Spec.t ; imap : string Mmap.t }

  module Omap =
  struct
    include Mmap
    let get k m = try Some(Mmap.find k m) with Not_found -> None
    let domain m =
      let s = ref S.empty in
      Mmap.iter (fun k _ -> s := S.add k !s) m ; !s

    let print msg m =
      let ds = domain m in
      Format.printf "%s:" msg ;
      S.iter
	(fun k ->
	   Format.printf "@\n + %04X: %S" k (find k m)) ds ;
      Format.print_newline ()
  end
      
  module CheckSpecMmap = Oracle(Spec)(Omap)
  module CheckCollect = Oracle(Spec)(Spec)
    
  type order = Increase | Decrease | Start | NoOrder

  let print_order fmt = function
    | Increase -> Format.fprintf fmt "increase"
    | Decrease -> Format.fprintf fmt "decrease"
    | Start    -> Format.fprintf fmt "????????"
    | NoOrder  -> Format.fprintf fmt "no-order"

  let check_ordering e = 
    let rel = ref Start
    and prev = ref None in
    let start () =
      rel := Start;
      prev := None 
    and check k = 
      (match !rel, !prev with
	 | Decrease, Some e when e > k -> ()
	 | Increase, Some e when e < k -> ()
	 | Start, Some e    when e < k -> rel := Increase
	 | Start, Some e    when e > k -> rel := Decrease
	 | _, Some e -> raise Not_found
	 | _, None -> assert (!rel = Start));
      prev := Some k
    in    
    let check_ordering msg siter iiter sfold ifold b =
      let check_ordering iter fold m =
	try
	  start ();
	  if b 
	  then ignore (iter (fun x _   -> check x) m)
	  else ignore (fold (fun x _ _ -> check x) m ());
	!rel
	with Not_found -> NoOrder
      in
      let srel = check_ordering siter sfold e.spec in
      let irel = check_ordering iiter ifold e.imap in 
	if srel = irel && srel != Start
	then Format.printf "%s: OK      : %a@." msg print_order srel
	else Format.printf "%s: KO      : %a / %a@." msg print_order srel print_order irel
    in check_ordering "Map-Order    " Spec.mapi Mmap.mapi Spec.fold Mmap.fold true;
      check_ordering "Iter-Order   " Spec.iter Mmap.iter Spec.fold Mmap.fold true;
      check_ordering "fold-Order   " Spec.iter Mmap.iter Spec.fold Mmap.fold false
      
  let empty = { spec = Spec.empty ; imap = Mmap.empty }
    
  let add e k d = {
    spec = Spec.add k d e.spec ;
    imap = Mmap.add k d e.imap ;
  }

  let remove e k = {
    spec = Spec.remove k e.spec ;
    imap = Mmap.remove k e.imap ;
  }

  let mix k d d' = Format.sprintf "%04X(%s,%s)" k d d'

  let partition0 k _d = (0 != k mod 5)
  let partition1 k d = not (partition0 k d)

  let map0 k d = if 0 != k mod 11 then Format.sprintf "%04X(%s)" k d else d
  let some_map0 k d = if partition0 k d then Some (map0 k d) else None
  let some_partition0 k d = if partition0 k d then Some d else None

  let some_mix1 k d d' = Some (Format.sprintf "%04X(%s,%s)" k d d')

  let some_mix2 k d d' = 
    if 0 != k mod 11 then 
      Some (Format.sprintf "%04X(%s,%s)" k d d')
    else
      None

  let some_diff_mix2 k d d' = 
    if 0 = k mod 5 then 
      Some (Format.sprintf "%04X(%s,%s)" k d d')
    else
      None

  let diff_mix2 k d d' = match d , d' with
    | None , None -> assert false
    | Some _ , None -> d
    | None , Some d' -> None
    | Some d , Some d' -> some_diff_mix2 k d d'


  let mix1 k d d' = match d , d' with
    | None , None -> assert false
    | Some d , None    when 0 != k mod 13 -> Some (Format.sprintf "%04X(%s,-)" k d)
    | None , Some _ -> None
    | Some d , Some d' when 0 != k mod 7 -> Some (Format.sprintf "%04X(%s,%s)" k d d')
    | _ , _ -> None

  let mix2 k d d' = match d , d' with
    | None , None -> assert false
    | Some _ , None -> None
    | None , Some d'   when 0 != k mod 13 -> Some (Format.sprintf "%04X(-,%s)" k d')
    | Some d , Some d' when 0 != k mod 11 -> Some (Format.sprintf "%04X(%s,%s)" k d d')
    | _ , _ -> None

  let mix3 k d d' = match d , d' with
    | None , None -> assert false
    | Some d , None  when 0 != k mod 7 -> Some (Format.sprintf "%04X(%s,-)" k d)
    | None , Some d' when 0 != k mod 11 -> Some (Format.sprintf "%04X(-,%s)" k d')
    | Some d , Some d' when 0 != k mod 13 -> Some (Format.sprintf "%04X(%s,%s)" k d d')
    | _ , _ -> None

  let utime () = (Unix.times ()).Unix.tms_utime

  let uexec delta fs fm ?(time=false) a =
    if not time then ( delta := None ; fs a.spec , fm a.imap)
    else
      let t0 = utime () in
      let s = fs a.spec in
      let t1 = utime () in
      let m = fm a.imap in
      let t2 = utime () in
      delta := Some( t1-.t0 , t2-.t1 ) ; s , m

  let binexec delta fs fm ?time a b =
    uexec delta (fs a.spec) (fm a.imap) ?time b
	
  let uopexec fs fm ?time a = 
    uexec CheckSpecMmap.timed fs fm ?time a

  let uop fs fm ?time a = 
    let s,m = uexec CheckSpecMmap.timed fs fm ?time a in { spec = s ; imap = m }

  let binop fs fm ?time a b = 
    let s,m = binexec CheckSpecMmap.timed fs fm ?time a b in { spec = s ; imap = m }

  let partition  = uopexec (Spec.filter partition0) (Mmap.partition partition0)
  let filter  = uop (Spec.filter partition1) (Mmap.filter partition1)
  let mapq1  = uop (Spec.filter partition0) (Mmap.mapq some_partition0)
  let mapq2  = uop (fun m -> Spec.mapi map0 (Spec.filter partition0 m)) (Mmap.mapq some_map0)
  let union  = binop (Spec.union mix) (Mmap.union mix)
  let inter  = binop (Spec.inter mix) (Mmap.inter mix)
  let interf1 = binop (Spec.inter mix) (Mmap.interf some_mix1)
  let interf2 = binop (Spec.interf some_mix2) (Mmap.interf some_mix2)
  let interq1 = binop (Spec.inter mix) (Mmap.interq some_mix1)
  let interq2 = binop (Spec.interf some_mix2) (Mmap.interq some_mix2)
  let diffq  = binop (Spec.merge diff_mix2) (Mmap.diffq some_diff_mix2)
  let merge1 = binop (Spec.merge mix1) (Mmap.merge mix1)
  let merge2 = binop (Spec.merge mix2) (Mmap.merge mix2)
  let merge3 = binop (Spec.merge mix3) (Mmap.merge mix3)

  let collect r k d d' =
    let u =
      match d , d' with
	| None , None -> Printf.sprintf "%04X:-" k
	| Some x , None -> Printf.sprintf "%04X:LEFT %s" k x
	| None , Some y -> Printf.sprintf "%04X:RIGHT %s" k y
	| Some x , Some y -> Printf.sprintf "%04X:BOTH %s %s" k x y
    in r := M.add k u !r

  let fcollect phi w1 w2 = let r = ref M.empty in phi (collect r) w1 w2 ; !r
  let iter2 = binexec CheckCollect.timed (fcollect Spec.iter2) (fcollect Mmap.iter2)

end

let section msg =
  begin
    Format.printf "======================================@\n" ;
    Format.printf "=== %s@\n" msg ;
    Format.printf "======================================@\n" ;
  end

module Test(Mmap : MergingMap) =
struct

  module M2 = Monitor(Mmap)
  open M2
      
  let add_array ?(debug=false) e ks =
    Array.fold_left
      (fun e k ->
	 let d = Data.data () in
	 let e = M2.add e k d in
	 if debug then
	   M2.CheckSpecMmap.check
	     (Printf.sprintf "Added %04X %S" k d) e.spec e.imap ;
	 e) e ks
      
  let remove_array ?(debug=false) e ks =
    Array.fold_left
      (fun e k ->
	 let e = M2.remove e k in
	 if debug then
	   M2.CheckSpecMmap.check
	     (Printf.sprintf "Removed %04X" k) e.spec e.imap ; 
	 e) e ks
      
  let debug = false

  let fill_and_empty () =
    begin
      let e = add_array ~debug M2.empty (Data.keys ()) in
      M2.CheckSpecMmap.check "Filled" e.spec e.imap ;
      let e = remove_array ~debug e (Data.keys ()) in
      M2.CheckSpecMmap.check "Empty" M.empty e.imap
    end
      
  let merging ?time e0 =
    begin
      Gc.compact () ;
      M2.CheckSpecMmap.check "Common" e0.spec e0.imap ;
      let ks1 = Array.map (fun x -> x + 1) Data.dkeys in
      let ks2 = Array.map (fun x -> x + 2) Data.dkeys in
      let e1 = add_array e0 ks1 in
      M2.check_ordering e1 ;
      M2.CheckSpecMmap.check "Map-1" e1.spec e1.imap ;
      let e2 = add_array e0 ks2 in
      M2.CheckSpecMmap.check "Map-2" e2.spec e2.imap ;
      let v = mapq1 ?time e1 in
      M2.CheckSpecMmap.check "MapQ-1" v.spec v.imap ;
      let v = mapq2 ?time e2 in
      M2.CheckSpecMmap.check "MapQ-2" v.spec v.imap ;
      let v = filter ?time e1 in
      M2.CheckSpecMmap.check "Filter" v.spec v.imap ;
      let sp,(m1,m2) = partition ?time e1 in
      M2.CheckSpecMmap.check "Partition-1" sp m1 ;
      M2.CheckSpecMmap.check "Partition-2" v.spec m2 ;
      let v = union ?time e1 e2 in
      M2.CheckSpecMmap.check "Union" v.spec v.imap ;
      let v = inter ?time e1 e2 in
      M2.CheckSpecMmap.check "Inter" v.spec v.imap ;
      let v = interf1 ?time e1 e2 in
      M2.CheckSpecMmap.check "InterF-1" v.spec v.imap ;
      let v = interf2 ?time e1 e2 in
      M2.CheckSpecMmap.check "InterF-2" v.spec v.imap ;
      let v = interq1 ?time e1 e2 in
      M2.CheckSpecMmap.check "InterQ-1" v.spec v.imap ;
      let v = interq2 ?time e1 e2 in
      M2.CheckSpecMmap.check "InterQ-2" v.spec v.imap ;
      let v = diffq ?time e1 e2 in
      M2.CheckSpecMmap.check "DiffQ" v.spec v.imap ;
      let v = merge1 ?time e1 e2 in
      M2.CheckSpecMmap.check "Merge-1" v.spec v.imap ;
      let v = merge2 ?time e1 e2 in
      M2.CheckSpecMmap.check "Merge-2" v.spec v.imap ;
      let v = merge3 ?time e1 e2 in
      M2.CheckSpecMmap.check "Merge-3" v.spec v.imap ;
      let v,w = iter2 ?time e1 e2 in
      M2.CheckCollect.check "Iter-2" v w ;
    end

  let bits = add_array M2.empty Data.dkeys

  let run title = 
    begin
      section title ;
      fill_and_empty () ;
      merging bits ;
    end

  let massive title n =
    begin
      section title ;
      Data.reset () ;
      let e = ref M2.empty in
      for i = 1 to n do
	let k = Data.int (2*n) in
	let d = Data.data () in
	e := M2.add !e k d
      done ;
      merging ~time:true !e ;
    end

end

module H1 = struct include I let hash _ = 1 end
module H2 = struct include I let hash x = (x mod 256) end
module H3 = struct include I let hash x = x end

module T1 = Test(struct include Qed.Intmap let iter=iteri let fold=foldi end)
module T2 = Test(Qed.Listmap.Make(I))
module T3 = Test(Qed.Mergemap.Make(H1))
module T4 = Test(Qed.Mergemap.Make(H2))
module T5 = Test(Qed.Mergemap.Make(H3))

let () =
  begin
    T1.run "*** Qed.Intmap ***" ;
    T2.run "*** Qed.Listmap ***" ;
    T3.run "*** Qed.Mergemap (poor hash) ***" ;
    T4.run "*** Qed.Mergemap (mid  hash) ***" ;
    T5.run "*** Qed.Mergemap (good hash) ***" ; 
    T1.massive "*** Qed.Intmap   (int  10^3) ***" 1000 ;
    T2.massive "*** Qed.Listmap  (list 10^3) ***" 1000 ;
    T3.massive "*** Qed.Mergemap (poor 10^3) ***" 1000 ;
    T4.massive "*** Qed.Mergemap (mid  10^3) ***" 1000 ;
    T5.massive "*** Qed.Mergemap (good 10^3) ***" 1000 ;
    T1.massive "*** Qed.Intmap   (int  10^4) ***" 10000 ;
    T2.massive "*** Qed.Listmap  (list 10^4) ***" 10000 ;
    T3.massive "*** Qed.Mergemap (poor 10^4) ***" 10000 ;
    T4.massive "*** Qed.Mergemap (mid  10^4) ***" 10000 ;
    T5.massive "*** Qed.Mergemap (good 10^4) ***" 10000 ;
    T1.massive "*** Qed.Intmap   (int  10^5) ***" 100000 ;
(*
    T2.massive "*** Qed.Listmap  (list 10^5) ***" 100000 ;
    T3.massive "*** Qed.Mergemap (poor 10^5) ***" 100000 ;
*)
    T4.massive "*** Qed.Mergemap (mid  10^5) ***" 100000 ;
    T5.massive "*** Qed.Mergemap (good 10^5) ***" 100000 ;
    print_endline "======================================" ;
  end
