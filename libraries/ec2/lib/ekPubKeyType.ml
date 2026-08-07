open Aws.BaseTypes

type t =
  | Rsa_2048
  | Ecc_sec_p384

let str_to_t = [ "ecc-sec-p384", Ecc_sec_p384; "rsa-2048", Rsa_2048 ]
let t_to_str = [ Ecc_sec_p384, "ecc-sec-p384"; Rsa_2048, "rsa-2048" ]
let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
