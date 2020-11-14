(* Auto-generated from "endpoints.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type service_defaults = Endpoints_t.service_defaults =
  { protocols : string list option
  ; ssl_common_name : string option
  }

type endpoint = Endpoints_t.endpoint = { hostname : string option }

type service = Endpoints_t.service =
  { defaults : service_defaults option
  ; endpoints : (string * endpoint) list
  }

type region = Endpoints_t.region = { description : string }

type partition_defaults = Endpoints_t.partition_defaults =
  { hostname : string option
  ; protocols : string list
  ; signature_versions : string list
  }

type partition = Endpoints_t.partition =
  { defaults : partition_defaults
  ; dns_suffix : string
  ; partition : string
  ; partition_name : string
  ; region_regex : string
  ; regions : (string * region) list
  ; services : (string * service) list
  }

type endpoints = Endpoints_t.endpoints = { partitions : partition list }

val write_service_defaults : Bi_outbuf.t -> service_defaults -> unit
(** Output a JSON value of type {!service_defaults}. *)

val string_of_service_defaults : ?len:int -> service_defaults -> string
(** Serialize a value of type {!service_defaults}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_service_defaults : Yojson.Safe.lexer_state -> Lexing.lexbuf -> service_defaults
(** Input JSON data of type {!service_defaults}. *)

val service_defaults_of_string : string -> service_defaults
(** Deserialize JSON data of type {!service_defaults}. *)

val write_endpoint : Bi_outbuf.t -> endpoint -> unit
(** Output a JSON value of type {!endpoint}. *)

val string_of_endpoint : ?len:int -> endpoint -> string
(** Serialize a value of type {!endpoint}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_endpoint : Yojson.Safe.lexer_state -> Lexing.lexbuf -> endpoint
(** Input JSON data of type {!endpoint}. *)

val endpoint_of_string : string -> endpoint
(** Deserialize JSON data of type {!endpoint}. *)

val write_service : Bi_outbuf.t -> service -> unit
(** Output a JSON value of type {!service}. *)

val string_of_service : ?len:int -> service -> string
(** Serialize a value of type {!service}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_service : Yojson.Safe.lexer_state -> Lexing.lexbuf -> service
(** Input JSON data of type {!service}. *)

val service_of_string : string -> service
(** Deserialize JSON data of type {!service}. *)

val write_region : Bi_outbuf.t -> region -> unit
(** Output a JSON value of type {!region}. *)

val string_of_region : ?len:int -> region -> string
(** Serialize a value of type {!region}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_region : Yojson.Safe.lexer_state -> Lexing.lexbuf -> region
(** Input JSON data of type {!region}. *)

val region_of_string : string -> region
(** Deserialize JSON data of type {!region}. *)

val write_partition_defaults : Bi_outbuf.t -> partition_defaults -> unit
(** Output a JSON value of type {!partition_defaults}. *)

val string_of_partition_defaults : ?len:int -> partition_defaults -> string
(** Serialize a value of type {!partition_defaults}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_partition_defaults :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> partition_defaults
(** Input JSON data of type {!partition_defaults}. *)

val partition_defaults_of_string : string -> partition_defaults
(** Deserialize JSON data of type {!partition_defaults}. *)

val write_partition : Bi_outbuf.t -> partition -> unit
(** Output a JSON value of type {!partition}. *)

val string_of_partition : ?len:int -> partition -> string
(** Serialize a value of type {!partition}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_partition : Yojson.Safe.lexer_state -> Lexing.lexbuf -> partition
(** Input JSON data of type {!partition}. *)

val partition_of_string : string -> partition
(** Deserialize JSON data of type {!partition}. *)

val write_endpoints : Bi_outbuf.t -> endpoints -> unit
(** Output a JSON value of type {!endpoints}. *)

val string_of_endpoints : ?len:int -> endpoints -> string
(** Serialize a value of type {!endpoints}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_endpoints : Yojson.Safe.lexer_state -> Lexing.lexbuf -> endpoints
(** Input JSON data of type {!endpoints}. *)

val endpoints_of_string : string -> endpoints
(** Deserialize JSON data of type {!endpoints}. *)
