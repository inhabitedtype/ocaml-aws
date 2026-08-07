open Aws.BaseTypes

type t =
  { policy : String.t option
  ; policy_name : String.t option
  }

let make ?policy ?policy_name () = { policy; policy_name }

let parse xml =
  Some
    { policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
    ; policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ; Aws.Util.option_map v.policy (fun f ->
             Aws.Query.Pair ("Policy", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ])

let of_json j =
  { policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  }
