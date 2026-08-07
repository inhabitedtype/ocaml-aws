open Aws.BaseTypes

type t = { hook_result_id : String.t option }

let make ?hook_result_id () = { hook_result_id }

let parse xml =
  Some
    { hook_result_id =
        Aws.Util.option_bind (Aws.Xml.member "HookResultId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hook_result_id (fun f ->
             Aws.Query.Pair ("HookResultId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hook_result_id (fun f -> "HookResultId", String.to_json f)
       ])

let of_json j =
  { hook_result_id = Aws.Util.option_map (Aws.Json.lookup j "HookResultId") String.of_json
  }
