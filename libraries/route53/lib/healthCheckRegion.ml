open Aws.BaseTypes

type t =
  | Us_east_1
  | Us_west_1
  | Us_west_2
  | Eu_west_1
  | Ap_southeast_1
  | Ap_southeast_2
  | Ap_northeast_1
  | Sa_east_1

let str_to_t =
  [ "sa-east-1", Sa_east_1
  ; "ap-northeast-1", Ap_northeast_1
  ; "ap-southeast-2", Ap_southeast_2
  ; "ap-southeast-1", Ap_southeast_1
  ; "eu-west-1", Eu_west_1
  ; "us-west-2", Us_west_2
  ; "us-west-1", Us_west_1
  ; "us-east-1", Us_east_1
  ]

let t_to_str =
  [ Sa_east_1, "sa-east-1"
  ; Ap_northeast_1, "ap-northeast-1"
  ; Ap_southeast_2, "ap-southeast-2"
  ; Ap_southeast_1, "ap-southeast-1"
  ; Eu_west_1, "eu-west-1"
  ; Us_west_2, "us-west-2"
  ; Us_west_1, "us-west-1"
  ; Us_east_1, "us-east-1"
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
