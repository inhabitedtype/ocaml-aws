open Aws.BaseTypes

type t =
  | MYSQL_NATIVE_PASSWORD
  | MYSQL_CACHING_SHA2_PASSWORD
  | POSTGRES_SCRAM_SHA_256
  | POSTGRES_MD5
  | SQL_SERVER_AUTHENTICATION

let str_to_t =
  [ "SQL_SERVER_AUTHENTICATION", SQL_SERVER_AUTHENTICATION
  ; "POSTGRES_MD5", POSTGRES_MD5
  ; "POSTGRES_SCRAM_SHA_256", POSTGRES_SCRAM_SHA_256
  ; "MYSQL_CACHING_SHA2_PASSWORD", MYSQL_CACHING_SHA2_PASSWORD
  ; "MYSQL_NATIVE_PASSWORD", MYSQL_NATIVE_PASSWORD
  ]

let t_to_str =
  [ SQL_SERVER_AUTHENTICATION, "SQL_SERVER_AUTHENTICATION"
  ; POSTGRES_MD5, "POSTGRES_MD5"
  ; POSTGRES_SCRAM_SHA_256, "POSTGRES_SCRAM_SHA_256"
  ; MYSQL_CACHING_SHA2_PASSWORD, "MYSQL_CACHING_SHA2_PASSWORD"
  ; MYSQL_NATIVE_PASSWORD, "MYSQL_NATIVE_PASSWORD"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
