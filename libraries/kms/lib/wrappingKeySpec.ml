open Aws.BaseTypes

type t =
  | RSA_2048
  | RSA_3072
  | RSA_4096
  | SM2

let str_to_t =
  [ "SM2", SM2; "RSA_4096", RSA_4096; "RSA_3072", RSA_3072; "RSA_2048", RSA_2048 ]

let t_to_str =
  [ SM2, "SM2"; RSA_4096, "RSA_4096"; RSA_3072, "RSA_3072"; RSA_2048, "RSA_2048" ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
