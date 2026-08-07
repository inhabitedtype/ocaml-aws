open Aws.BaseTypes

type t =
  { resource_arn : String.t option
  ; audit_policy_state : AuditPolicyState.t option
  }

let make ?resource_arn ?audit_policy_state () = { resource_arn; audit_policy_state }

let parse xml =
  Some
    { resource_arn = Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse
    ; audit_policy_state =
        Aws.Util.option_bind
          (Aws.Xml.member "AuditPolicyState" xml)
          AuditPolicyState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.audit_policy_state (fun f ->
             Aws.Query.Pair ("AuditPolicyState", AuditPolicyState.to_query f))
       ; Aws.Util.option_map v.resource_arn (fun f ->
             Aws.Query.Pair ("ResourceArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.audit_policy_state (fun f ->
             "AuditPolicyState", AuditPolicyState.to_json f)
       ; Aws.Util.option_map v.resource_arn (fun f -> "ResourceArn", String.to_json f)
       ])

let of_json j =
  { resource_arn = Aws.Util.option_map (Aws.Json.lookup j "ResourceArn") String.of_json
  ; audit_policy_state =
      Aws.Util.option_map (Aws.Json.lookup j "AuditPolicyState") AuditPolicyState.of_json
  }
