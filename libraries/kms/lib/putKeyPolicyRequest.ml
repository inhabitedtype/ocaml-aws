open Aws.BaseTypes

type t =
  { key_id : String.t
  ; policy_name : String.t option
  ; policy : String.t
  ; bypass_policy_lockout_safety_check : Boolean.t option
  }

let make ~key_id ?policy_name ~policy ?bypass_policy_lockout_safety_check () =
  { key_id; policy_name; policy; bypass_policy_lockout_safety_check }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    ; policy =
        Aws.Xml.required
          "Policy"
          (Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse)
    ; bypass_policy_lockout_safety_check =
        Aws.Util.option_bind
          (Aws.Xml.member "BypassPolicyLockoutSafetyCheck" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
             Aws.Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Policy", String.to_query v.policy))
       ; Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
             "BypassPolicyLockoutSafetyCheck", Boolean.to_json f)
       ; Some ("Policy", String.to_json v.policy)
       ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  ; policy = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Policy"))
  ; bypass_policy_lockout_safety_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "BypassPolicyLockoutSafetyCheck")
        Boolean.of_json
  }
