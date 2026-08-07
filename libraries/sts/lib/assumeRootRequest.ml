open Aws.BaseTypes

type t =
  { target_principal : String.t
  ; task_policy_arn : PolicyDescriptorType.t
  ; duration_seconds : Integer.t option
  }

let make ~target_principal ~task_policy_arn ?duration_seconds () =
  { target_principal; task_policy_arn; duration_seconds }

let parse xml =
  Some
    { target_principal =
        Aws.Xml.required
          "TargetPrincipal"
          (Aws.Util.option_bind (Aws.Xml.member "TargetPrincipal" xml) String.parse)
    ; task_policy_arn =
        Aws.Xml.required
          "TaskPolicyArn"
          (Aws.Util.option_bind
             (Aws.Xml.member "TaskPolicyArn" xml)
             PolicyDescriptorType.parse)
    ; duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.duration_seconds (fun f ->
             Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TaskPolicyArn", PolicyDescriptorType.to_query v.task_policy_arn))
       ; Some (Aws.Query.Pair ("TargetPrincipal", String.to_query v.target_principal))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ; Some ("TaskPolicyArn", PolicyDescriptorType.to_json v.task_policy_arn)
       ; Some ("TargetPrincipal", String.to_json v.target_principal)
       ])

let of_json j =
  { target_principal =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetPrincipal"))
  ; task_policy_arn =
      PolicyDescriptorType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TaskPolicyArn"))
  ; duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  }
