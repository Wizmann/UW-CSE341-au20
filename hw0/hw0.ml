(* CSE 341 hw0.ml *)

(* Edit this file to address all the TODOs. *)

(* Here is a subset of the file we worked on during lecture 1. *)

let x = 34

let y = 17

(* TODO: change this according to the description of problem 1 below. *)
let z = -(x + y) + (y + 2)

let abs_of_z = if z < 0 then 0 - z else z

(*

Problem 1:

Change the expression being bound to z above so that it produces a negative number.

Evaluate your code using OCaml, and confirm that abs_of_z is still positive, as expected.

The only point of this problem is just to give you a reason to install OCaml.
So, you should actually test your code by running it through OCaml!

You can read more about how to run OCaml code by reading the course notes
or by attending section on 10/1/20.

As a challenge, try to do it by making the smallest change possible.

*)

(* Turn in the resulting file in Gradescope. *)

