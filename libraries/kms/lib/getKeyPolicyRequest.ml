open Aws.BaseTypes

type t =
  { key_id : String.t
  ; policy_name : String.t option
  }

let make ~key_id ?policy_name () = { key_id; policy_name }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  }
