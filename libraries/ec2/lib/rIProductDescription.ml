open Aws.BaseTypes

type t =
  | Linux_UNIX
  | Linux_UNIX__Amazon_VPC_
  | Windows
  | Windows__Amazon_VPC_

let str_to_t =
  [ "Windows (Amazon VPC)", Windows__Amazon_VPC_
  ; "Windows", Windows
  ; "Linux/UNIX (Amazon VPC)", Linux_UNIX__Amazon_VPC_
  ; "Linux/UNIX", Linux_UNIX
  ]

let t_to_str =
  [ Windows__Amazon_VPC_, "Windows (Amazon VPC)"
  ; Windows, "Windows"
  ; Linux_UNIX__Amazon_VPC_, "Linux/UNIX (Amazon VPC)"
  ; Linux_UNIX, "Linux/UNIX"
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
