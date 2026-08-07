open Aws.BaseTypes

type t =
  | Us_east_1
  | Us_east_2
  | Us_west_1
  | Us_west_2
  | Ca_central_1
  | Eu_west_1
  | Eu_west_2
  | Eu_west_3
  | Eu_central_1
  | Eu_central_2
  | Ap_southeast_1
  | Ap_southeast_2
  | Ap_southeast_3
  | Ap_northeast_1
  | Ap_northeast_2
  | Ap_northeast_3
  | Eu_north_1
  | Sa_east_1
  | Cn_north_1
  | Cn_northwest_1
  | Ap_east_1
  | Me_south_1
  | Me_central_1
  | Ap_south_1
  | Ap_south_2
  | Af_south_1
  | Eu_south_1
  | Eu_south_2
  | Ap_southeast_4
  | Il_central_1
  | Ca_west_1
  | Ap_southeast_5
  | Mx_central_1
  | Ap_southeast_7
  | Us_gov_east_1
  | Us_gov_west_1
  | Ap_east_2
  | Ap_southeast_6
  | Eusc_de_east_1

let str_to_t =
  [ "eusc-de-east-1", Eusc_de_east_1
  ; "ap-southeast-6", Ap_southeast_6
  ; "ap-east-2", Ap_east_2
  ; "us-gov-west-1", Us_gov_west_1
  ; "us-gov-east-1", Us_gov_east_1
  ; "ap-southeast-7", Ap_southeast_7
  ; "mx-central-1", Mx_central_1
  ; "ap-southeast-5", Ap_southeast_5
  ; "ca-west-1", Ca_west_1
  ; "il-central-1", Il_central_1
  ; "ap-southeast-4", Ap_southeast_4
  ; "eu-south-2", Eu_south_2
  ; "eu-south-1", Eu_south_1
  ; "af-south-1", Af_south_1
  ; "ap-south-2", Ap_south_2
  ; "ap-south-1", Ap_south_1
  ; "me-central-1", Me_central_1
  ; "me-south-1", Me_south_1
  ; "ap-east-1", Ap_east_1
  ; "cn-northwest-1", Cn_northwest_1
  ; "cn-north-1", Cn_north_1
  ; "sa-east-1", Sa_east_1
  ; "eu-north-1", Eu_north_1
  ; "ap-northeast-3", Ap_northeast_3
  ; "ap-northeast-2", Ap_northeast_2
  ; "ap-northeast-1", Ap_northeast_1
  ; "ap-southeast-3", Ap_southeast_3
  ; "ap-southeast-2", Ap_southeast_2
  ; "ap-southeast-1", Ap_southeast_1
  ; "eu-central-2", Eu_central_2
  ; "eu-central-1", Eu_central_1
  ; "eu-west-3", Eu_west_3
  ; "eu-west-2", Eu_west_2
  ; "eu-west-1", Eu_west_1
  ; "ca-central-1", Ca_central_1
  ; "us-west-2", Us_west_2
  ; "us-west-1", Us_west_1
  ; "us-east-2", Us_east_2
  ; "us-east-1", Us_east_1
  ]

let t_to_str =
  [ Eusc_de_east_1, "eusc-de-east-1"
  ; Ap_southeast_6, "ap-southeast-6"
  ; Ap_east_2, "ap-east-2"
  ; Us_gov_west_1, "us-gov-west-1"
  ; Us_gov_east_1, "us-gov-east-1"
  ; Ap_southeast_7, "ap-southeast-7"
  ; Mx_central_1, "mx-central-1"
  ; Ap_southeast_5, "ap-southeast-5"
  ; Ca_west_1, "ca-west-1"
  ; Il_central_1, "il-central-1"
  ; Ap_southeast_4, "ap-southeast-4"
  ; Eu_south_2, "eu-south-2"
  ; Eu_south_1, "eu-south-1"
  ; Af_south_1, "af-south-1"
  ; Ap_south_2, "ap-south-2"
  ; Ap_south_1, "ap-south-1"
  ; Me_central_1, "me-central-1"
  ; Me_south_1, "me-south-1"
  ; Ap_east_1, "ap-east-1"
  ; Cn_northwest_1, "cn-northwest-1"
  ; Cn_north_1, "cn-north-1"
  ; Sa_east_1, "sa-east-1"
  ; Eu_north_1, "eu-north-1"
  ; Ap_northeast_3, "ap-northeast-3"
  ; Ap_northeast_2, "ap-northeast-2"
  ; Ap_northeast_1, "ap-northeast-1"
  ; Ap_southeast_3, "ap-southeast-3"
  ; Ap_southeast_2, "ap-southeast-2"
  ; Ap_southeast_1, "ap-southeast-1"
  ; Eu_central_2, "eu-central-2"
  ; Eu_central_1, "eu-central-1"
  ; Eu_west_3, "eu-west-3"
  ; Eu_west_2, "eu-west-2"
  ; Eu_west_1, "eu-west-1"
  ; Ca_central_1, "ca-central-1"
  ; Us_west_2, "us-west-2"
  ; Us_west_1, "us-west-1"
  ; Us_east_2, "us-east-2"
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
