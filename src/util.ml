(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Structures

module Printing = struct
  let with_output file f =
    let out = open_out file in
    try
      f out;
      flush out;
      close_out out
    with _ -> close_out out

  let write_all ~filename contents =
    with_output filename (fun out -> output_string out contents)

  let write_structure filename es = write_all ~filename (Pprintast.string_of_structure es)

  let string_of_signature sign =
    ignore (Format.flush_str_formatter ());
    Pprintast.signature Format.str_formatter sign;
    Format.flush_str_formatter ()

  let write_signature filename sign = write_all ~filename (string_of_signature sign)
end

module StringTable = Map.Make (String)

module Char = struct
  include Char

  let _is_uppercase i = i >= 65 && i <= 90

  let _is_alpha i = _is_uppercase i || (i >= 97 && i <= 122)

  let is_uppercase c = _is_uppercase (code c)

  let is_alpha c = _is_alpha (code c)

  let is_alphanum c =
    let i = code c in
    _is_alpha i || (i >= 48 && i <= 57)
end

let is_reserved_keyword token =
  try
    List.assoc
      token
      [ "and", true
      ; "as", true
      ; "assert", true
      ; "begin", true
      ; "class", true
      ; "constraint", true
      ; "do", true
      ; "done", true
      ; "downto", true
      ; "else", true
      ; "end", true
      ; "exception", true
      ; "external", true
      ; "false", true
      ; "for", true
      ; "fun", true
      ; "function", true
      ; "functor", true
      ; "if", true
      ; "in", true
      ; "include", true
      ; "inherit", true
      ; "inherit!", true
      ; "initializer", true
      ; "lazy", true
      ; "let", true
      ; "match", true
      ; "method", true
      ; "method!", true
      ; "module", true
      ; "mutable", true
      ; "new", true
      ; "object", true
      ; "of", true
      ; "open", true
      ; "or", true
      ; "private", true
      ; "rec", true
      ; "sig", true
      ; "struct", true
      ; "then", true
      ; "to", true
      ; "true", true
      ; "try", true
      ; "type", true
      ; "val", true
      ; "val!", true
      ; "virtual", true
      ; "when", true
      ; "while", true
      ; "with", true
      ]
  with Not_found -> false

let to_variant_name s =
  String.map
    (fun c -> if Char.is_alphanum c then c else '_')
    (if Char.is_alpha s.[0] then String.capitalize_ascii s else "N" ^ s)

let to_field_name s =
  let add_underscore = ref false in
  let acc = ref [] in
  String.iter
    (fun c ->
      let c' = Printf.sprintf "%c" Char.(lowercase_ascii c) in
      let s =
        if Char.is_uppercase c && !add_underscore
        then "_" ^ c'
        else (
          add_underscore := true;
          c')
      in
      acc := s :: !acc)
    s;
  let res = String.concat "" (List.rev !acc) in
  if is_reserved_keyword res then res ^ "_" else res

(* NOTE(dbp 2015-01-26): Shapes that just have primitive types
   (boolean, integer, etc) types aren't actually useful (they
   communicate no information, since all of the typing is
   structural). Some string types are `enums`, which we support by translating
   them into actual types, but non-enum string shapes are similarly not
   useful. So, we will both remove references to them in fields (by
   just replacing them with Integer, Boolean, etc), and removing the
   shapes themselves.

   NOTE(dbp 2015-02-04): Another optimization: get rid of structs with no fields.
   Many of these are actually error descriptions, but once we support those, they
   would get parsed before this pass (similarly to how enums don't get inlined
   away as strings).
*)
let inline_shapes (ops : Operation.t list) (shapes : Shape.parsed StringTable.t) =
  let type_map =
    (* NOTE(dbp 2015-01-26): Not all of these have been seen in the
       wild (ex: double, float, datetime) *)
    [ "boolean", "Boolean"
    ; "string", "String"
    ; "integer", "Integer"
    ; "long", "Long"
    ; "double", "Double"
    ; "float", "Float"
    ; "datetime", "DateTime"
      (* NOTE(dbp 2015-01-26): timestamp is a type used in the
         CreatedTime shape for elasticloadbalancing, and as far as I can
         tell from the examples, it is the same as DateTime. *)
    ; "timestamp", "DateTime"
    ; "blob", "Blob"
    ]
  in
  let replace_shape default =
    try
      let _, shptyp, _ = StringTable.find default shapes in
      List.assoc shptyp type_map
    with Not_found -> default
  in
  let new_shapes =
    StringTable.fold
      (fun key (nm, ty, contents) acc ->
        if List.mem_assoc ty type_map
        then acc
        else
          let content =
            match contents with
            | None -> assert false
            | Some (Shape.Structure ms) ->
                Shape.Structure
                  (List.map
                     (fun member ->
                       { member with
                         Structure.shape = replace_shape member.Structure.shape
                       })
                     ms)
            | Some (Shape.List (shp, ln, flatten)) ->
                Shape.List (replace_shape shp, ln, flatten)
            | Some (Shape.Map ((kshp, kln), (vshp, vln))) ->
                Shape.Map ((replace_shape kshp, kln), (replace_shape vshp, vln))
            | Some (Shape.Enum opts) -> Shape.Enum opts
          in
          StringTable.add key { Shape.name = nm; content } acc)
      shapes
      StringTable.empty
  in
  let is_empty_struct shp =
    try
      match StringTable.find shp shapes with
      | _, _, Some (Shape.Structure []) -> true
      | _ -> false
    with Not_found -> false
  in
  let new_ops =
    List.map
      (fun op ->
        let input_shape =
          match op.Operation.input_shape with
          | Some shp when is_empty_struct shp -> None
          | shp -> shp
        and output_shape =
          match op.Operation.output_shape with
          | Some shp when is_empty_struct shp -> None
          | shp -> shp
        in
        { op with Operation.input_shape; output_shape })
      ops
  in
  new_shapes, new_ops

let rec filter_map l ~f =
  match l with
  | [] -> []
  | x :: xs -> (
      match f x with
      | Some x' -> x' :: filter_map xs ~f
      | None -> filter_map xs ~f)

let option_map l ~f =
  match l with
  | None -> None
  | Some x -> Some (f ^ x)

let of_option_exn = function
  | Some v -> v
  | None -> failwith "Expected Some v, got None."
