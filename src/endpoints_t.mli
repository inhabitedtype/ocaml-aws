(* Auto-generated from "endpoints.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type raw_json = Yojson.Safe.json

type constraint_op = [
    `STARTS_WITH | `NOT_EQUALS | `EQUALS | `ONE_OF | `NOT_STARTS_WITH
]

type constraint_on = [ `REGION ]

type constraint_data = { data: string option list }

type constraint_ = (constraint_on * constraint_op * constraint_data)

type endpoint = { uri: string; constraints: constraint_ list option }

type endpoints = (string * endpoint list) list
