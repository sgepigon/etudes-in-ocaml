
open Core.Std

(** A collection of strings frequency counts *)
type t

(** The empty set of frequency counts *)
val empty : t

(** Converts the set of frequency counts to an association list.
    A string shows up at most once, and the counts are >= 1. *)
val to_list : t -> (string * int) list

(** Bump the frequenct count for the given string. *)
val touch : t -> string -> t
(** Type mismatch example; ordering.
  val touch : t -> string -> t 
*)

(** Causes trouble without an implementation
  val count : t -> string -> int 
*)



