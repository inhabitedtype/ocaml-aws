open Query
open Xml

module type Base = sig
  type t

  val to_json : t -> Json.t

  val of_json : Json.t -> t

  val to_query : t -> Query.t

  val parse : Ezxmlm.nodes -> t option

  val to_string : t -> string

  val of_string : string -> t
end

module Unit = struct
  type t = unit

  let to_json () = `Null

  let of_json = function
    | `Null -> ()
    | t -> raise (Json.Casting_error ("unit", t))

  let to_query () = List []

  let parse _ = Some () (* XXX(seliopou): Should never be used, maybe assert that? *)

  let to_string _ = raise (Failure "unit")

  let of_string _ = raise (Failure "unit")
end

module String = struct
  include String

  let to_json s = `String s

  let of_json = function
    | `String s -> s
    | t -> raise (Json.Casting_error ("string", t))

  let to_query s = Value (Some s)

  let parse s = Some (data_to_string s)

  let to_string s = s

  let of_string s = s
end

(* NOTE(dbp 2015-01-15): In EC2, Blobs seem to be used for Base64
   encoded data, which seems okay to represent as a string, at least
   for now. *)
module Blob = String

module Boolean = struct
  type t = bool

  let to_json b = `Bool b

  let of_json = function
    | `Bool b -> b
    | t -> raise (Json.Casting_error ("bool", t))

  let to_query = function
    | true -> Value (Some "true")
    | false -> Value (Some "false")

  let parse b =
    match String.parse b with
    | None -> None
    | Some s -> (
        match String.lowercase_ascii s with
        | "false" -> Some false
        | "true" -> Some true
        | _ -> None)

  let to_string b = if b then "true" else "false"

  let of_string s =
    match String.lowercase_ascii s with
    | "false" -> false
    | "true" -> true
    | _ -> raise (Failure ("Bad boolean string " ^ s))
end

module Integer = struct
  type t = int

  let to_json i = `Int i

  let of_json = function
    | `Int i -> i
    | t -> raise (Json.Casting_error ("int", t))

  let to_query i = Value (Some (string_of_int i))

  let parse i =
    match String.parse i with
    | None -> None
    | Some s -> ( try Some (int_of_string s) with Failure _ -> None)

  let to_string i = string_of_int i

  let of_string s = int_of_string s
end

module Long = Integer

module Float = struct
  type t = float

  let to_json f = `Float f

  let of_json = function
    | `Float f -> f
    | t -> raise (Json.Casting_error ("float", t))

  let to_query f = Value (Some (string_of_float f))

  let parse f =
    match String.parse f with
    | None -> None
    | Some s -> ( try Some (float_of_string s) with Failure _ -> None)

  let to_string f = string_of_float f

  let of_string s = float_of_string s
end

module Double = Float

module DateTime = struct
  type t = CalendarLib.Calendar.t

  let to_json c = `String (Time.format c)

  let of_json t = Time.parse (String.of_json t)

  let to_query c = Value (Some (Time.format c))

  let parse c =
    match String.parse c with
    | None -> None
    | Some s -> ( try Some (Time.parse s) with Invalid_argument _ -> None)

  let to_string c = Time.format c

  let of_string s = Time.parse s
end
