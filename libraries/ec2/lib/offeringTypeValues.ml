open Aws.BaseTypes

type t =
  | Heavy_Utilization
  | Medium_Utilization
  | Light_Utilization
  | No_Upfront
  | Partial_Upfront
  | All_Upfront

let str_to_t =
  [ "All Upfront", All_Upfront
  ; "Partial Upfront", Partial_Upfront
  ; "No Upfront", No_Upfront
  ; "Light Utilization", Light_Utilization
  ; "Medium Utilization", Medium_Utilization
  ; "Heavy Utilization", Heavy_Utilization
  ]

let t_to_str =
  [ All_Upfront, "All Upfront"
  ; Partial_Upfront, "Partial Upfront"
  ; No_Upfront, "No Upfront"
  ; Light_Utilization, "Light Utilization"
  ; Medium_Utilization, "Medium Utilization"
  ; Heavy_Utilization, "Heavy Utilization"
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
