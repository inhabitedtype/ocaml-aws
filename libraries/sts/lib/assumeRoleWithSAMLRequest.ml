open Aws.BaseTypes

type t =
  { role_arn : String.t
  ; principal_arn : String.t
  ; s_a_m_l_assertion : String.t
  ; policy_arns : PolicyDescriptorListType.t
  ; policy : String.t option
  ; duration_seconds : Integer.t option
  }

let make
    ~role_arn
    ~principal_arn
    ~s_a_m_l_assertion
    ?(policy_arns = [])
    ?policy
    ?duration_seconds
    () =
  { role_arn; principal_arn; s_a_m_l_assertion; policy_arns; policy; duration_seconds }

let parse xml =
  Some
    { role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; principal_arn =
        Aws.Xml.required
          "PrincipalArn"
          (Aws.Util.option_bind (Aws.Xml.member "PrincipalArn" xml) String.parse)
    ; s_a_m_l_assertion =
        Aws.Xml.required
          "SAMLAssertion"
          (Aws.Util.option_bind (Aws.Xml.member "SAMLAssertion" xml) String.parse)
    ; policy_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyArns" xml)
             PolicyDescriptorListType.parse)
    ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
    ; duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.duration_seconds (fun f ->
             Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
       ; Aws.Util.option_map v.policy (fun f ->
             Aws.Query.Pair ("Policy", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PolicyArns.member", PolicyDescriptorListType.to_query v.policy_arns))
       ; Some (Aws.Query.Pair ("SAMLAssertion", String.to_query v.s_a_m_l_assertion))
       ; Some (Aws.Query.Pair ("PrincipalArn", String.to_query v.principal_arn))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ; Some ("PolicyArns", PolicyDescriptorListType.to_json v.policy_arns)
       ; Some ("SAMLAssertion", String.to_json v.s_a_m_l_assertion)
       ; Some ("PrincipalArn", String.to_json v.principal_arn)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ])

let of_json j =
  { role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; principal_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrincipalArn"))
  ; s_a_m_l_assertion =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SAMLAssertion"))
  ; policy_arns =
      PolicyDescriptorListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyArns"))
  ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  }
