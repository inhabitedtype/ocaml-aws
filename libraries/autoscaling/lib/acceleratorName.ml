open Aws.BaseTypes

type t =
  | A100
  | V100
  | K80
  | T4
  | M60
  | Radeon_pro_v520
  | Vu9p

let str_to_t =
  [ "vu9p", Vu9p
  ; "radeon-pro-v520", Radeon_pro_v520
  ; "m60", M60
  ; "t4", T4
  ; "k80", K80
  ; "v100", V100
  ; "a100", A100
  ]

let t_to_str =
  [ Vu9p, "vu9p"
  ; Radeon_pro_v520, "radeon-pro-v520"
  ; M60, "m60"
  ; T4, "t4"
  ; K80, "k80"
  ; V100, "v100"
  ; A100, "a100"
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
