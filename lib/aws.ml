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

module Util = struct
  let drop_empty l = List.filter (fun p -> String.length (String.trim (fst p)) <> 0) l

  let or_error v err =
    match v with
    | None -> `Error err
    | Some v -> `Ok v

  let of_option default = function
    | None -> default
    | Some v -> v

  let of_option_exn = function
    | Some v -> v
    | None -> failwith "Expected Some v, got None."

  let rec list_find l key =
    match l with
    | [] -> None
    | (k, v) :: xs -> if k = key then Some v else list_find xs key

  let rec list_filter_opt = function
    | [] -> []
    | Some v :: xs -> v :: list_filter_opt xs
    | None :: xs -> list_filter_opt xs

  let option_bind o f =
    match o with
    | None -> None
    | Some v -> f v

  let option_map v f =
    match v with
    | None -> None
    | Some v -> Some (f v)

  let rec option_all = function
    | [] -> Some []
    | Some v :: xs -> option_bind (option_all xs) (fun rest -> Some (v :: rest))
    | None :: _ -> None
end

module Xml = struct
  exception RequiredFieldMissing of string

  let member tag xml =
    try Some (Ezxmlm.member tag xml) with Ezxmlm.Tag_not_found _ -> None

  let members tag xml = try Ezxmlm.members tag xml with Ezxmlm.Tag_not_found _ -> []

  let data_to_string = Ezxmlm.data_to_string

  let required nm a =
    match a with
    | Some v -> v
    | None -> raise (RequiredFieldMissing nm)
end

module Error = struct
  type 'a code =
    | Understood of 'a
    | Unknown of string

  type bad_response =
    { body : string
    ; message : string
    }

  type 'a error_response =
    | BadResponse of bad_response
    | AwsError of ('a code * string) list

  type 'a t =
    | TransportError of string
    | HttpError of int * 'a error_response

  let code_to_string utos = function
    | Understood code -> utos code
    | Unknown code -> code

  let format print_native = function
    | TransportError msg -> Printf.(sprintf "TransportError %s" msg)
    | HttpError (code, err) -> (
        match err with
        | BadResponse br ->
            Printf.sprintf
              "HttpError(%d - BadResponse): %s. Body: %s\n"
              code
              br.message
              br.body
        | AwsError ers ->
            Printf.sprintf
              "HttpError(%d - AwsError): %s"
              code
              (String.concat
                 ", "
                 (List.map
                    (fun (code, msg) ->
                      Printf.sprintf "[%s: %s]" (code_to_string print_native code) msg)
                    ers)))

  let parse_aws_error body =
    try
      let tags = Ezxmlm.from_string body |> snd in
      let errors =
        Util.(
          match
            option_bind (Xml.member "Response" tags) (fun r ->
                option_bind (Xml.member "Errors" r) (fun errs ->
                    Some (Xml.members "Error" errs)))
          with
          | Some es -> Some es
          | None ->
              option_bind (Xml.member "ErrorResponse" tags) (fun r ->
                  Some (Xml.members "Error" r)))
      in
      match errors with
      | None -> `Error "Could not find <Error> nodes for error response code."
      | Some err_nodes ->
          Util.(
            option_map
              (List.map
                 (fun node ->
                   match
                     ( option_map (Xml.member "Code" node) Xml.data_to_string
                     , option_map (Xml.member "Message" node) Xml.data_to_string )
                   with
                   | Some error_code, Some message -> Some (error_code, message)
                   | _ -> None)
                 err_nodes
              |> option_all)
              (fun res -> `Ok res)
            |> of_option
                 (`Error
                   "Could not find properly formatted <Error> nodes in <Errors> response."))
    with Failure msg -> `Error ("Error parsing xml: " ^ msg)
end

module Request = struct
  type meth =
    [ `DELETE
    | `GET
    | `HEAD
    | `OPTIONS
    | `CONNECT
    | `TRACE
    | `Other of string
    | `PATCH
    | `POST
    | `PUT
    ]

  let string_of_meth = function
    | `DELETE -> "DELETE"
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `OPTIONS -> "OPTIONS"
    | `CONNECT -> "CONNECT"
    | `TRACE -> "TRACE"
    | `Other s -> s
    | `PATCH -> "PATCH"
    | `POST -> "POST"
    | `PUT -> "PUT"

  type headers = (string * string) list

  type signature_version =
    | V4
    | V2
    | S3

  type t = meth * Uri.t * headers
end

module type Call = sig
  type input

  type output

  type error

  val signature_version : Request.signature_version

  val service : string

  val to_http : string -> string -> input -> Request.t

  val of_http : string -> [ `Ok of output | `Error of error Error.error_response ]

  val parse_error : int -> string -> error option
end

type ('i, 'o, 'e) call =
  (module Call with type input = 'i and type output = 'o and type error = 'e)

module Time = struct
  module C = CalendarLib.Calendar
  module P = CalendarLib.Printer.Calendar

  let date_yymmdd = P.sprint "%Y%m%d"

  let date_time_iso8601 = P.sprint "%Y-%m-%dT%H:%M:%S"

  let date_time = P.sprint "%Y%m%dT%H%M%SZ"

  let now_utc () = C.(now () |> to_gmt)

  (* (tmcgilchrist) This function is expecting datetimes like
      - "2021-03-17T21:43:32.000Z" from EC2 or
      - "2021-03-18T09:38:33Z" from STS
     We regex off the trailing ".000" and parse them. If there are other
     datetime formats in xml / json there will be trouble and the parser
     will fail with xml node not present or json attribute not present.
  *)
  let parse s =
    P.from_fstring
      "%Y-%m-%dT%TZ"
      (Str.replace_first (Str.regexp "\\.\\([0-9][0-9][0-9]\\)") "" s)

  let format t = P.sprint "%Y-%m-%dT%T.000Z" t
end

module Query = struct
  type t =
    | List of t list
    | Pair of (string * t)
    | Value of string option

  let render q =
    let rec enc k q =
      match k, q with
      | k, List xs -> List.concat (List.map (enc k) xs)
      | Some n, Pair (label, subq) -> enc (Some (n ^ "." ^ label)) subq
      | None, Pair (label, subq) -> enc (Some label) subq
      | Some n, Value (Some s) -> [ n ^ "=" ^ Uri.pct_encode ~component:`Query_value s ]
      | None, Value (Some s) -> [ Uri.pct_encode s ]
      | Some s, _ -> [ s ]
      | _ -> []
    in
    String.concat "&" (enc None q)

  let to_query_list to_query vals =
    let i = ref 0 in
    List
      (List.map
         (fun v ->
           i := !i + 1;
           Pair (string_of_int !i, to_query v))
         vals)

  let to_query_hashtbl key_to_str to_query tbl =
    List (Hashtbl.fold (fun k v acc -> Pair (key_to_str k, to_query v) :: acc) tbl [])
end

module Json = struct
  type t =
    [ `Assoc of (string * t) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of t list
    | `Null
    | `String of string
    ]

  exception Casting_error of string * t

  let to_list f = function
    | `List l -> List.map f l
    | t -> raise (Casting_error ("list", t))

  let to_hashtbl key_f f = function
    | `Assoc m ->
        List.fold_left
          (fun acc (k, v) ->
            Hashtbl.add acc (key_f k) (f v);
            acc)
          (Hashtbl.create (List.length m))
          m
    | t -> raise (Casting_error ("map", t))

  let lookup t s =
    try
      match t with
      | `Assoc l -> Some (List.assoc s l)
      | _ -> raise Not_found
    with Not_found -> None
end

module BaseTypes = struct
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
end

module Endpoints = Endpoints

module Signing = struct
  module Hash = struct
    let _sha256 ?key str =
      match key with
      | Some key -> Digestif.SHA256.hmac_string ~key str
      | None -> Digestif.SHA256.digest_string str

    let sha256 ?key str = _sha256 ?key str |> Digestif.SHA256.to_raw_string

    let sha256_hex ?key str = _sha256 ?key str |> Digestif.SHA256.to_hex

    let sha256_base64 ?key str = Base64.encode_string @@ sha256 ?key str
  end

  let encode_query ps =
    (* NOTE(dbp 2015-03-13): We want just:
       A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ), and tilde ( ~ ).
              As per the docs:
              http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
              Uri has that as it's fall-through, which at least currently (and hopefully forever)
              ~component:`Authority causes it to use.
    *)
    let encoded =
      List.map
        (fun (k, v) ->
          let key = Uri.pct_encode ~component:`Authority k in
          let value =
            match v with
            | [] -> ""
            | [ x ] -> Uri.pct_encode ~component:`Authority x
            | _ -> failwith "AWS query cannot have multiple values for same key"
          in
          key, value)
        ps
    in
    let sorted = List.sort (fun a b -> compare (fst a) (fst b)) encoded in
    let joined = List.map (fun (k, v) -> k ^ "=" ^ v) sorted in
    String.concat "&" joined

  (* NOTE(dbp 2015-01-13): This is a direct translation of reference implementation at:
   * http://docs.aws.amazon.com/general/latest/gr/sigv4-signed-request-examples.html
   *)
  let sign_request ~access_key ~secret_key ?token ~service ~region (meth, uri, headers) =
    let host = Util.of_option_exn (Endpoints.endpoint_of service region) in
    let params = encode_query (Uri.query uri) in
    let sign key msg = Hash.sha256 ~key msg in
    let get_signature_key key date region service =
      sign (sign (sign (sign ("AWS4" ^ key) date) region) service) "aws4_request"
    in
    let now = Time.now_utc () in
    let amzdate = Time.date_time now in
    let datestamp = Time.date_yymmdd now in
    let canonical_uri = "/" in
    let canonical_querystring = params in
    let payload_hash = Hash.sha256_hex "" in
    let token_header, sig_header =
      match token with
      | Some t ->
          let th = "x-amz-security-token:" ^ t ^ "\n" in
          let sh = ";x-amz-security-token" in
          th, sh
      | None -> "", ""
    in
    let canonical_headers =
      "host:"
      ^ host
      ^ "\n"
      ^ "x-amz-content-sha256:"
      ^ payload_hash
      ^ "\nx-amz-date:"
      ^ amzdate
      ^ "\n"
      ^ token_header
    in
    let signed_headers = "host;x-amz-content-sha256;x-amz-date" ^ sig_header in
    let canonical_request =
      Request.string_of_meth meth
      ^ "\n"
      ^ canonical_uri
      ^ "\n"
      ^ canonical_querystring
      ^ "\n"
      ^ canonical_headers
      ^ "\n"
      ^ signed_headers
      ^ "\n"
      ^ payload_hash
    in
    let algorithm = "AWS4-HMAC-SHA256" in
    let credential_scope =
      datestamp ^ "/" ^ region ^ "/" ^ service ^ "/" ^ "aws4_request"
    in
    let string_to_sign =
      algorithm
      ^ "\n"
      ^ amzdate
      ^ "\n"
      ^ credential_scope
      ^ "\n"
      ^ Hash.sha256_hex canonical_request
    in
    let signing_key = get_signature_key secret_key datestamp region service in
    let signature = Hash.sha256_hex ~key:signing_key string_to_sign in
    let authorization_header =
      String.concat
        ""
        [ algorithm
        ; " "
        ; "Credential="
        ; access_key
        ; "/"
        ; credential_scope
        ; ", "
        ; "SignedHeaders="
        ; signed_headers
        ; ", "
        ; "Signature="
        ; signature
        ]
    in
    let headers =
      ("x-amz-date", amzdate)
      :: ("x-amz-content-sha256", payload_hash)
      :: ("Authorization", authorization_header)
      :: headers
    in
    let full_headers =
      match token with
      | Some t -> ("X-Amz-Security-Token", t) :: headers
      | None -> headers
    in
    meth, uri, full_headers

  let sign_v2_request ~access_key ~secret_key ?token ~service ~region (meth, uri, headers)
      =
    let host = Util.of_option_exn (Endpoints.endpoint_of service region) in
    let amzdate = Time.date_time_iso8601 (Time.now_utc ()) in

    let query =
      Uri.add_query_params'
        uri
        ((match token with
         | Some t -> [ "SecurityToken", t ]
         | None -> [])
        @ [ "Timestamp", amzdate
          ; "AWSAccessKeyId", access_key
          ; "SignatureMethod", "HmacSHA256"
          ; "SignatureVersion", "2"
          ])
    in

    let params = encode_query (Uri.query query) in
    let canonical_uri = "/" in
    let string_to_sign =
      Request.string_of_meth meth ^ "\n" ^ host ^ "\n" ^ canonical_uri ^ "\n" ^ params
    in
    let signature = Base64.encode_string @@ Hash.sha256 ~key:secret_key string_to_sign in
    let new_uri = Uri.add_query_param' query ("Signature", signature) in
    meth, new_uri, headers
end
