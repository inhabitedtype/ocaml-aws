open Aws.BaseTypes

type t =
  | Attaching
  | Attached
  | Detaching
  | Detached
  | Attach_failed
  | Detach_failed

let str_to_t =
  [ "detach-failed", Detach_failed
  ; "attach-failed", Attach_failed
  ; "detached", Detached
  ; "detaching", Detaching
  ; "attached", Attached
  ; "attaching", Attaching
  ]

let t_to_str =
  [ Detach_failed, "detach-failed"
  ; Attach_failed, "attach-failed"
  ; Detached, "detached"
  ; Detaching, "detaching"
  ; Attached, "attached"
  ; Attaching, "attaching"
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
