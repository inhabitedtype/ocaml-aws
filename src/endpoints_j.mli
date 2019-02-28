(* Auto-generated from "endpoints.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type raw_json = Yojson.Safe.json

type constraint_op = Endpoints_t.constraint_op

type constraint_on = Endpoints_t.constraint_on

type constraint_data = Endpoints_t.constraint_data = {
  data: string option list
}

type constraint_ = Endpoints_t.constraint_

type endpoint = Endpoints_t.endpoint = {
  uri: string;
  constraints: constraint_ list option
}

type endpoints = Endpoints_t.endpoints

val write_raw_json :
  Bi_outbuf.t -> raw_json -> unit
  (** Output a JSON value of type {!raw_json}. *)

val string_of_raw_json :
  ?len:int -> raw_json -> string
  (** Serialize a value of type {!raw_json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_raw_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> raw_json
  (** Input JSON data of type {!raw_json}. *)

val raw_json_of_string :
  string -> raw_json
  (** Deserialize JSON data of type {!raw_json}. *)

val write_constraint_op :
  Bi_outbuf.t -> constraint_op -> unit
  (** Output a JSON value of type {!constraint_op}. *)

val string_of_constraint_op :
  ?len:int -> constraint_op -> string
  (** Serialize a value of type {!constraint_op}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_constraint_op :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> constraint_op
  (** Input JSON data of type {!constraint_op}. *)

val constraint_op_of_string :
  string -> constraint_op
  (** Deserialize JSON data of type {!constraint_op}. *)

val write_constraint_on :
  Bi_outbuf.t -> constraint_on -> unit
  (** Output a JSON value of type {!constraint_on}. *)

val string_of_constraint_on :
  ?len:int -> constraint_on -> string
  (** Serialize a value of type {!constraint_on}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_constraint_on :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> constraint_on
  (** Input JSON data of type {!constraint_on}. *)

val constraint_on_of_string :
  string -> constraint_on
  (** Deserialize JSON data of type {!constraint_on}. *)

val write_constraint_data :
  Bi_outbuf.t -> constraint_data -> unit
  (** Output a JSON value of type {!constraint_data}. *)

val string_of_constraint_data :
  ?len:int -> constraint_data -> string
  (** Serialize a value of type {!constraint_data}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_constraint_data :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> constraint_data
  (** Input JSON data of type {!constraint_data}. *)

val constraint_data_of_string :
  string -> constraint_data
  (** Deserialize JSON data of type {!constraint_data}. *)

val write_constraint_ :
  Bi_outbuf.t -> constraint_ -> unit
  (** Output a JSON value of type {!constraint_}. *)

val string_of_constraint_ :
  ?len:int -> constraint_ -> string
  (** Serialize a value of type {!constraint_}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_constraint_ :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> constraint_
  (** Input JSON data of type {!constraint_}. *)

val constraint__of_string :
  string -> constraint_
  (** Deserialize JSON data of type {!constraint_}. *)

val write_endpoint :
  Bi_outbuf.t -> endpoint -> unit
  (** Output a JSON value of type {!endpoint}. *)

val string_of_endpoint :
  ?len:int -> endpoint -> string
  (** Serialize a value of type {!endpoint}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_endpoint :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> endpoint
  (** Input JSON data of type {!endpoint}. *)

val endpoint_of_string :
  string -> endpoint
  (** Deserialize JSON data of type {!endpoint}. *)

val write_endpoints :
  Bi_outbuf.t -> endpoints -> unit
  (** Output a JSON value of type {!endpoints}. *)

val string_of_endpoints :
  ?len:int -> endpoints -> string
  (** Serialize a value of type {!endpoints}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_endpoints :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> endpoints
  (** Input JSON data of type {!endpoints}. *)

val endpoints_of_string :
  string -> endpoints
  (** Deserialize JSON data of type {!endpoints}. *)

