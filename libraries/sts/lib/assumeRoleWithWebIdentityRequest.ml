open Aws.BaseTypes

type t =
  { role_arn : String.t
  ; role_session_name : String.t
  ; web_identity_token : String.t
  ; provider_id : String.t option
  ; policy_arns : PolicyDescriptorListType.t
  ; policy : String.t option
  ; duration_seconds : Integer.t option
  }

let make
    ~role_arn
    ~role_session_name
    ~web_identity_token
    ?provider_id
    ?(policy_arns = [])
    ?policy
    ?duration_seconds
    () =
  { role_arn
  ; role_session_name
  ; web_identity_token
  ; provider_id
  ; policy_arns
  ; policy
  ; duration_seconds
  }

let parse xml =
  Some
    { role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; role_session_name =
        Aws.Xml.required
          "RoleSessionName"
          (Aws.Util.option_bind (Aws.Xml.member "RoleSessionName" xml) String.parse)
    ; web_identity_token =
        Aws.Xml.required
          "WebIdentityToken"
          (Aws.Util.option_bind (Aws.Xml.member "WebIdentityToken" xml) String.parse)
    ; provider_id = Aws.Util.option_bind (Aws.Xml.member "ProviderId" xml) String.parse
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
       ; Aws.Util.option_map v.provider_id (fun f ->
             Aws.Query.Pair ("ProviderId", String.to_query f))
       ; Some (Aws.Query.Pair ("WebIdentityToken", String.to_query v.web_identity_token))
       ; Some (Aws.Query.Pair ("RoleSessionName", String.to_query v.role_session_name))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ; Some ("PolicyArns", PolicyDescriptorListType.to_json v.policy_arns)
       ; Aws.Util.option_map v.provider_id (fun f -> "ProviderId", String.to_json f)
       ; Some ("WebIdentityToken", String.to_json v.web_identity_token)
       ; Some ("RoleSessionName", String.to_json v.role_session_name)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ])

let of_json j =
  { role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; role_session_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleSessionName"))
  ; web_identity_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "WebIdentityToken"))
  ; provider_id = Aws.Util.option_map (Aws.Json.lookup j "ProviderId") String.of_json
  ; policy_arns =
      PolicyDescriptorListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyArns"))
  ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  }
