(* CSE341 HW3 Provided Code *)

(* function composition operator from lecture *)
let (%) f g x = f (g x)

exception NoAnswer

type pattern =
  | WildcardP
  | VariableP of string
  | UnitP
  | ConstantP of int
  | ConstructorP of string * pattern
  | TupleP of pattern list

type valu =
  | Constant of int
  | Unit
  | Constructor of string * valu
  | Tuple of valu list

let rec g f1 f2 p =
  let r = g f1 f2 in
  match p with
  | WildcardP         -> f1 ()
  | VariableP x       -> f2 x
  | ConstructorP(_,p) -> r p
  | TupleP ps         -> List.fold_left (fun i p -> (r p) + i) 0 ps
  | _                 -> 0

(**** for the challenge problem only ****)

type typ =
  | AnythingT
  | UnitT
  | IntT
  | TupleT of typ list
  | VariantT of string
