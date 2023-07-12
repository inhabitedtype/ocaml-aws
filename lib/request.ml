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
