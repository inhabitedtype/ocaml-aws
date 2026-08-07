open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; stack_policy_body : String.t option
  ; stack_policy_u_r_l : String.t option
  }

let make ~stack_name ?stack_policy_body ?stack_policy_u_r_l () =
  { stack_name; stack_policy_body; stack_policy_u_r_l }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; stack_policy_body =
        Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
    ; stack_policy_u_r_l =
        Aws.Util.option_bind (Aws.Xml.member "StackPolicyURL" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
             Aws.Query.Pair ("StackPolicyURL", String.to_query f))
       ; Aws.Util.option_map v.stack_policy_body (fun f ->
             Aws.Query.Pair ("StackPolicyBody", String.to_query f))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
             "StackPolicyURL", String.to_json f)
       ; Aws.Util.option_map v.stack_policy_body (fun f ->
             "StackPolicyBody", String.to_json f)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; stack_policy_body =
      Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
  ; stack_policy_u_r_l =
      Aws.Util.option_map (Aws.Json.lookup j "StackPolicyURL") String.of_json
  }
