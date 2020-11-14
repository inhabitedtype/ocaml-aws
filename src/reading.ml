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

module Json = struct
  include Yojson.Basic.Util

  let member_exn name json =
    let r = Yojson.Basic.Util.member name json in
    if r = `Null then raise Not_found else r

  let member_opt name json =
    match Yojson.Basic.Util.member name json with
    | exception _ -> None
    | `Null -> None
    | r -> Some r
end

let unreserve =
  let reserved_words = [ "type", "type_"; "to", "to_"; "end", "end_" ] in
  fun s -> try List.assoc s reserved_words with Not_found -> s

let parse_member rq (mnm, mj) =
  { Structure.name = mnm
  ; shape = Json.(member_exn "shape" mj |> to_string)
  ; loc_name =
      (match Json.member "locationName" mj with
      | `Null -> None
      | loc -> Some (Json.to_string loc))
  ; required = List.mem mnm rq
  ; field_name = unreserve (Util.to_field_name mnm)
  }

let shape ((nm, j) : string * Yojson.Basic.t) : Shape.parsed =
  match Json.member "type" j with
  | `String "structure" ->
      let required =
        match Json.member "required" j with
        | `Null -> []
        | required -> List.map Json.to_string (Json.to_list required)
      in
      let member = Json.(member_exn "members" j |> to_assoc) in
      nm, "structure", Some (Shape.Structure (List.map (parse_member required) member))
  | `String "list" ->
      let member = Json.member_exn "member" j in
      let flattened =
        match Json.member "flattened" j with
        | `Bool true -> true
        | _ -> false
      in
      let shape = Json.member_exn "shape" member |> Json.to_string in
      let loc_name =
        match Json.member "locationName" member with
        | `Null -> None
        | loc_name -> Some (Json.to_string loc_name)
      in
      nm, "list", Some (Shape.List (shape, loc_name, flattened))
  | `String "map" ->
      let key = Json.member_exn "key" j in
      let key_shape = Json.member_exn "shape" key |> Json.to_string in
      let value = Json.member_exn "value" j in
      let value_shape = Json.member_exn "shape" value |> Json.to_string in
      let key_loc_name =
        match Json.member "locationName" key with
        | `Null -> None
        | loc_name -> Some (Json.to_string loc_name)
      in
      let value_loc_name =
        match Json.member "locationName" value with
        | `Null -> None
        | loc_name -> Some (Json.to_string loc_name)
      in
      ( nm
      , "map"
      , Some (Shape.Map ((key_shape, key_loc_name), (value_shape, value_loc_name))) )
  | `String ty ->
      if ty = "string" && Json.member "enum" j <> `Null
      then
        let enum = Json.(member_exn "enum" j |> to_list) in
        let options = List.map Json.to_string enum in
        nm, "enum", Some (Shape.Enum options)
      else nm, ty, None
  | _ -> failwith (Printf.sprintf "Couldn't find 'type' on shape with name '%s'" nm)

let op (_nm, j) : Operation.t =
  let name = Json.(member_exn "name" j |> to_string) in
  let http = Json.member_exn "http" j in
  let http_meth = Json.(member_exn "method" http |> to_string) in
  let http_uri = Json.(member_exn "requestUri" http |> to_string) in
  let input_shape =
    (* XXX(seliopou): There are some API calls that take no arguments. For
     * example, [DescribeAccountLimits] from the autoscaling API is one such
     * call. As a quick fix, the aws library now includes [unit] in its base
     * types, and the input shape for API calls that do not take arguments now
     * directly references that type. This decision should be revisited in the
     * future.
     *)
    match Json.member "input" j with
    | `Null -> Some "Aws.BaseTypes.Unit"
    | input -> Some Json.(member_exn "shape" input |> to_string)
  in
  let output_shape =
    try Some Json.(member "output" j |> member_exn "shape" |> to_string) with _ -> None
  in
  let output_wrapper =
    try Some Json.(member "output" j |> member_exn "resultWrapper" |> to_string)
    with _ -> None
  in
  let errors =
    match Json.member "errors" j with
    | `Null -> []
    | errors ->
        Util.filter_map (Json.to_list errors) ~f:(fun e ->
            try Some Json.(member "error" e |> member "code" |> to_string)
            with _ -> None)
  in
  { Operation.name
  ; http_meth
  ; http_uri
  ; input_shape
  ; output_shape
  ; output_wrapper
  ; errors
  }

(* there might be a weird bug here that is not generating the errors *)
let error shape_name json =
  try
    let error = Json.member "error" json in
    let code =
      match Json.member_opt "code" error with
      | Some c -> Json.to_string c
      | None -> shape_name
    in
    let http_code =
      match Json.member "httpStatusCode" error with
      | `Int n -> Some n
      | `Null -> None
      | exception _ -> None
      | _ -> assert false
    in
    { Error.shape_name
    ; string_name = code
    ; variant_name = Util.to_variant_name code
    ; http_code
    }
  with _ ->
    failwith
      (Printf.sprintf
         "Couldn't parse error %s (fields missing or malformed): '%s'"
         shape_name
         (Yojson.Basic.to_string json))
