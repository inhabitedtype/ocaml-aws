open Aws.BaseTypes

type t =
  | Standard
  | Io1
  | Io2
  | Gp2
  | Sc1
  | St1
  | Gp3

let str_to_t =
  [ "gp3", Gp3
  ; "st1", St1
  ; "sc1", Sc1
  ; "gp2", Gp2
  ; "io2", Io2
  ; "io1", Io1
  ; "standard", Standard
  ]

let t_to_str =
  [ Gp3, "gp3"
  ; St1, "st1"
  ; Sc1, "sc1"
  ; Gp2, "gp2"
  ; Io2, "io2"
  ; Io1, "io1"
  ; Standard, "standard"
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
