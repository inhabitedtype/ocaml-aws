open Aws.BaseTypes

type t = { stack_policy_body : String.t option }

let make ?stack_policy_body () = { stack_policy_body }

let parse xml =
  Some
    { stack_policy_body =
        Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_policy_body (fun f ->
             Aws.Query.Pair ("StackPolicyBody", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_policy_body (fun f ->
             "StackPolicyBody", String.to_json f)
       ])

let of_json j =
  { stack_policy_body =
      Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
  }
