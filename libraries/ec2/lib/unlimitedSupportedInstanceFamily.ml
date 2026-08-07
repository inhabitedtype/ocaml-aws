open Aws.BaseTypes

type t =
  | T2
  | T3
  | T3a
  | T4g

let str_to_t = [ "t4g", T4g; "t3a", T3a; "t3", T3; "t2", T2 ]
let t_to_str = [ T4g, "t4g"; T3a, "t3a"; T3, "t3"; T2, "t2" ]
let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
