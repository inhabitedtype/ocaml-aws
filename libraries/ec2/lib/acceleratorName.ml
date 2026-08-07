open Aws.BaseTypes

type t =
  | A100
  | Inferentia
  | K520
  | K80
  | M60
  | Radeon_pro_v520
  | T4
  | Vu9p
  | V100
  | A10g
  | H100
  | T4g
  | L40s
  | L4
  | Gaudi_hl_205
  | Inferentia2
  | Trainium
  | Trainium2
  | U30

let str_to_t =
  [ "u30", U30
  ; "trainium2", Trainium2
  ; "trainium", Trainium
  ; "inferentia2", Inferentia2
  ; "gaudi-hl-205", Gaudi_hl_205
  ; "l4", L4
  ; "l40s", L40s
  ; "t4g", T4g
  ; "h100", H100
  ; "a10g", A10g
  ; "v100", V100
  ; "vu9p", Vu9p
  ; "t4", T4
  ; "radeon-pro-v520", Radeon_pro_v520
  ; "m60", M60
  ; "k80", K80
  ; "k520", K520
  ; "inferentia", Inferentia
  ; "a100", A100
  ]

let t_to_str =
  [ U30, "u30"
  ; Trainium2, "trainium2"
  ; Trainium, "trainium"
  ; Inferentia2, "inferentia2"
  ; Gaudi_hl_205, "gaudi-hl-205"
  ; L4, "l4"
  ; L40s, "l40s"
  ; T4g, "t4g"
  ; H100, "h100"
  ; A10g, "a10g"
  ; V100, "v100"
  ; Vu9p, "vu9p"
  ; T4, "t4"
  ; Radeon_pro_v520, "radeon-pro-v520"
  ; M60, "m60"
  ; K80, "k80"
  ; K520, "k520"
  ; Inferentia, "inferentia"
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
