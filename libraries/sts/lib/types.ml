open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module PolicyDescriptorType = struct
  type t = { arn : String.t option }

  let make ?arn () = { arn }

  let parse xml =
    Some { arn = Aws.Util.option_bind (Aws.Xml.member "arn" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("arn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.arn (fun f -> "arn", String.to_json f) ])

  let of_json j = { arn = Aws.Util.option_map (Aws.Json.lookup j "arn") String.of_json }
end

module Tag = struct
  type t =
    { key : String.t
    ; value : String.t
    }

  let make ~key ~value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", String.to_json v.value); Some ("Key", String.to_json v.key) ])

  let of_json j =
    { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module TagListType = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module TagKeyListType = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module PolicyDescriptorListType = struct
  type t = PolicyDescriptorType.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map PolicyDescriptorType.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PolicyDescriptorType.to_query v

  let to_json v = `List (List.map PolicyDescriptorType.to_json v)

  let of_json j = Aws.Json.to_list PolicyDescriptorType.of_json j
end

module AssumeRoleRequest = struct
  type t =
    { role_arn : String.t
    ; role_session_name : String.t
    ; policy_arns : PolicyDescriptorListType.t
    ; policy : String.t option
    ; duration_seconds : Integer.t option
    ; tags : TagListType.t
    ; transitive_tag_keys : TagKeyListType.t
    ; external_id : String.t option
    ; serial_number : String.t option
    ; token_code : String.t option
    }

  let make
      ~role_arn
      ~role_session_name
      ?(policy_arns = [])
      ?policy
      ?duration_seconds
      ?(tags = [])
      ?(transitive_tag_keys = [])
      ?external_id
      ?serial_number
      ?token_code
      () =
    { role_arn
    ; role_session_name
    ; policy_arns
    ; policy
    ; duration_seconds
    ; tags
    ; transitive_tag_keys
    ; external_id
    ; serial_number
    ; token_code
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
      ; policy_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyArns" xml)
               PolicyDescriptorListType.parse)
      ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
      ; duration_seconds =
          Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagListType.parse)
      ; transitive_tag_keys =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TransitiveTagKeys" xml)
               TagKeyListType.parse)
      ; external_id = Aws.Util.option_bind (Aws.Xml.member "ExternalId" xml) String.parse
      ; serial_number =
          Aws.Util.option_bind (Aws.Xml.member "SerialNumber" xml) String.parse
      ; token_code = Aws.Util.option_bind (Aws.Xml.member "TokenCode" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.token_code (fun f ->
               Aws.Query.Pair ("TokenCode", String.to_query f))
         ; Aws.Util.option_map v.serial_number (fun f ->
               Aws.Query.Pair ("SerialNumber", String.to_query f))
         ; Aws.Util.option_map v.external_id (fun f ->
               Aws.Query.Pair ("ExternalId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("TransitiveTagKeys.member", TagKeyListType.to_query v.transitive_tag_keys))
         ; Some (Aws.Query.Pair ("Tags.member", TagListType.to_query v.tags))
         ; Aws.Util.option_map v.duration_seconds (fun f ->
               Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
         ; Aws.Util.option_map v.policy (fun f ->
               Aws.Query.Pair ("Policy", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("PolicyArns.member", PolicyDescriptorListType.to_query v.policy_arns))
         ; Some (Aws.Query.Pair ("RoleSessionName", String.to_query v.role_session_name))
         ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.token_code (fun f -> "TokenCode", String.to_json f)
         ; Aws.Util.option_map v.serial_number (fun f -> "SerialNumber", String.to_json f)
         ; Aws.Util.option_map v.external_id (fun f -> "ExternalId", String.to_json f)
         ; Some ("TransitiveTagKeys", TagKeyListType.to_json v.transitive_tag_keys)
         ; Some ("Tags", TagListType.to_json v.tags)
         ; Aws.Util.option_map v.duration_seconds (fun f ->
               "DurationSeconds", Integer.to_json f)
         ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
         ; Some ("PolicyArns", PolicyDescriptorListType.to_json v.policy_arns)
         ; Some ("RoleSessionName", String.to_json v.role_session_name)
         ; Some ("RoleArn", String.to_json v.role_arn)
         ])

  let of_json j =
    { role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
    ; role_session_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleSessionName"))
    ; policy_arns =
        PolicyDescriptorListType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyArns"))
    ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
    ; duration_seconds =
        Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
    ; tags = TagListType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; transitive_tag_keys =
        TagKeyListType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitiveTagKeys"))
    ; external_id = Aws.Util.option_map (Aws.Json.lookup j "ExternalId") String.of_json
    ; serial_number =
        Aws.Util.option_map (Aws.Json.lookup j "SerialNumber") String.of_json
    ; token_code = Aws.Util.option_map (Aws.Json.lookup j "TokenCode") String.of_json
    }
end

module AssumeRoleWithWebIdentityRequest = struct
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
         ; Some
             (Aws.Query.Pair ("WebIdentityToken", String.to_query v.web_identity_token))
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
end

module GetCallerIdentityResponse = struct
  type t =
    { user_id : String.t option
    ; account : String.t option
    ; arn : String.t option
    }

  let make ?user_id ?account ?arn () = { user_id; account; arn }

  let parse xml =
    Some
      { user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
      ; account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
      ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ; Aws.Util.option_map v.account (fun f ->
               Aws.Query.Pair ("Account", String.to_query f))
         ; Aws.Util.option_map v.user_id (fun f ->
               Aws.Query.Pair ("UserId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
         ; Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
         ])

  let of_json j =
    { user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
    ; account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
    ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    }
end

module Credentials = struct
  type t =
    { access_key_id : String.t
    ; secret_access_key : String.t
    ; session_token : String.t
    ; expiration : DateTime.t
    }

  let make ~access_key_id ~secret_access_key ~session_token ~expiration () =
    { access_key_id; secret_access_key; session_token; expiration }

  let parse xml =
    Some
      { access_key_id =
          Aws.Xml.required
            "AccessKeyId"
            (Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse)
      ; secret_access_key =
          Aws.Xml.required
            "SecretAccessKey"
            (Aws.Util.option_bind (Aws.Xml.member "SecretAccessKey" xml) String.parse)
      ; session_token =
          Aws.Xml.required
            "SessionToken"
            (Aws.Util.option_bind (Aws.Xml.member "SessionToken" xml) String.parse)
      ; expiration =
          Aws.Xml.required
            "Expiration"
            (Aws.Util.option_bind (Aws.Xml.member "Expiration" xml) DateTime.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Expiration", DateTime.to_query v.expiration))
         ; Some (Aws.Query.Pair ("SessionToken", String.to_query v.session_token))
         ; Some (Aws.Query.Pair ("SecretAccessKey", String.to_query v.secret_access_key))
         ; Some (Aws.Query.Pair ("AccessKeyId", String.to_query v.access_key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Expiration", DateTime.to_json v.expiration)
         ; Some ("SessionToken", String.to_json v.session_token)
         ; Some ("SecretAccessKey", String.to_json v.secret_access_key)
         ; Some ("AccessKeyId", String.to_json v.access_key_id)
         ])

  let of_json j =
    { access_key_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessKeyId"))
    ; secret_access_key =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecretAccessKey"))
    ; session_token =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SessionToken"))
    ; expiration =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Expiration"))
    }
end

module AssumedRoleUser = struct
  type t =
    { assumed_role_id : String.t
    ; arn : String.t
    }

  let make ~assumed_role_id ~arn () = { assumed_role_id; arn }

  let parse xml =
    Some
      { assumed_role_id =
          Aws.Xml.required
            "AssumedRoleId"
            (Aws.Util.option_bind (Aws.Xml.member "AssumedRoleId" xml) String.parse)
      ; arn =
          Aws.Xml.required
            "Arn"
            (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
         ; Some (Aws.Query.Pair ("AssumedRoleId", String.to_query v.assumed_role_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Arn", String.to_json v.arn)
         ; Some ("AssumedRoleId", String.to_json v.assumed_role_id)
         ])

  let of_json j =
    { assumed_role_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AssumedRoleId"))
    ; arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
    }
end

module AssumeRoleWithWebIdentityResponse = struct
  type t =
    { credentials : Credentials.t option
    ; subject_from_web_identity_token : String.t option
    ; assumed_role_user : AssumedRoleUser.t option
    ; packed_policy_size : Integer.t option
    ; provider : String.t option
    ; audience : String.t option
    }

  let make
      ?credentials
      ?subject_from_web_identity_token
      ?assumed_role_user
      ?packed_policy_size
      ?provider
      ?audience
      () =
    { credentials
    ; subject_from_web_identity_token
    ; assumed_role_user
    ; packed_policy_size
    ; provider
    ; audience
    }

  let parse xml =
    Some
      { credentials =
          Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
      ; subject_from_web_identity_token =
          Aws.Util.option_bind
            (Aws.Xml.member "SubjectFromWebIdentityToken" xml)
            String.parse
      ; assumed_role_user =
          Aws.Util.option_bind
            (Aws.Xml.member "AssumedRoleUser" xml)
            AssumedRoleUser.parse
      ; packed_policy_size =
          Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
      ; provider = Aws.Util.option_bind (Aws.Xml.member "Provider" xml) String.parse
      ; audience = Aws.Util.option_bind (Aws.Xml.member "Audience" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.audience (fun f ->
               Aws.Query.Pair ("Audience", String.to_query f))
         ; Aws.Util.option_map v.provider (fun f ->
               Aws.Query.Pair ("Provider", String.to_query f))
         ; Aws.Util.option_map v.packed_policy_size (fun f ->
               Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
         ; Aws.Util.option_map v.assumed_role_user (fun f ->
               Aws.Query.Pair ("AssumedRoleUser", AssumedRoleUser.to_query f))
         ; Aws.Util.option_map v.subject_from_web_identity_token (fun f ->
               Aws.Query.Pair ("SubjectFromWebIdentityToken", String.to_query f))
         ; Aws.Util.option_map v.credentials (fun f ->
               Aws.Query.Pair ("Credentials", Credentials.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.audience (fun f -> "Audience", String.to_json f)
         ; Aws.Util.option_map v.provider (fun f -> "Provider", String.to_json f)
         ; Aws.Util.option_map v.packed_policy_size (fun f ->
               "PackedPolicySize", Integer.to_json f)
         ; Aws.Util.option_map v.assumed_role_user (fun f ->
               "AssumedRoleUser", AssumedRoleUser.to_json f)
         ; Aws.Util.option_map v.subject_from_web_identity_token (fun f ->
               "SubjectFromWebIdentityToken", String.to_json f)
         ; Aws.Util.option_map v.credentials (fun f ->
               "Credentials", Credentials.to_json f)
         ])

  let of_json j =
    { credentials =
        Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
    ; subject_from_web_identity_token =
        Aws.Util.option_map
          (Aws.Json.lookup j "SubjectFromWebIdentityToken")
          String.of_json
    ; assumed_role_user =
        Aws.Util.option_map (Aws.Json.lookup j "AssumedRoleUser") AssumedRoleUser.of_json
    ; packed_policy_size =
        Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
    ; provider = Aws.Util.option_map (Aws.Json.lookup j "Provider") String.of_json
    ; audience = Aws.Util.option_map (Aws.Json.lookup j "Audience") String.of_json
    }
end

module AssumeRoleWithSAMLRequest = struct
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
end

module GetSessionTokenRequest = struct
  type t =
    { duration_seconds : Integer.t option
    ; serial_number : String.t option
    ; token_code : String.t option
    }

  let make ?duration_seconds ?serial_number ?token_code () =
    { duration_seconds; serial_number; token_code }

  let parse xml =
    Some
      { duration_seconds =
          Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
      ; serial_number =
          Aws.Util.option_bind (Aws.Xml.member "SerialNumber" xml) String.parse
      ; token_code = Aws.Util.option_bind (Aws.Xml.member "TokenCode" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.token_code (fun f ->
               Aws.Query.Pair ("TokenCode", String.to_query f))
         ; Aws.Util.option_map v.serial_number (fun f ->
               Aws.Query.Pair ("SerialNumber", String.to_query f))
         ; Aws.Util.option_map v.duration_seconds (fun f ->
               Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.token_code (fun f -> "TokenCode", String.to_json f)
         ; Aws.Util.option_map v.serial_number (fun f -> "SerialNumber", String.to_json f)
         ; Aws.Util.option_map v.duration_seconds (fun f ->
               "DurationSeconds", Integer.to_json f)
         ])

  let of_json j =
    { duration_seconds =
        Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
    ; serial_number =
        Aws.Util.option_map (Aws.Json.lookup j "SerialNumber") String.of_json
    ; token_code = Aws.Util.option_map (Aws.Json.lookup j "TokenCode") String.of_json
    }
end

module InvalidAuthorizationMessageException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetSessionTokenResponse = struct
  type t = { credentials : Credentials.t option }

  let make ?credentials () = { credentials }

  let parse xml =
    Some
      { credentials =
          Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.credentials (fun f ->
               Aws.Query.Pair ("Credentials", Credentials.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.credentials (fun f ->
               "Credentials", Credentials.to_json f)
         ])

  let of_json j =
    { credentials =
        Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
    }
end

module DecodeAuthorizationMessageRequest = struct
  type t = { encoded_message : String.t }

  let make ~encoded_message () = { encoded_message }

  let parse xml =
    Some
      { encoded_message =
          Aws.Xml.required
            "EncodedMessage"
            (Aws.Util.option_bind (Aws.Xml.member "EncodedMessage" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("EncodedMessage", String.to_query v.encoded_message)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EncodedMessage", String.to_json v.encoded_message) ])

  let of_json j =
    { encoded_message =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EncodedMessage"))
    }
end

module GetAccessKeyInfoResponse = struct
  type t = { account : String.t option }

  let make ?account () = { account }

  let parse xml =
    Some { account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.account (fun f ->
               Aws.Query.Pair ("Account", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.account (fun f -> "Account", String.to_json f) ])

  let of_json j =
    { account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json }
end

module AssumeRoleResponse = struct
  type t =
    { credentials : Credentials.t option
    ; assumed_role_user : AssumedRoleUser.t option
    ; packed_policy_size : Integer.t option
    }

  let make ?credentials ?assumed_role_user ?packed_policy_size () =
    { credentials; assumed_role_user; packed_policy_size }

  let parse xml =
    Some
      { credentials =
          Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
      ; assumed_role_user =
          Aws.Util.option_bind
            (Aws.Xml.member "AssumedRoleUser" xml)
            AssumedRoleUser.parse
      ; packed_policy_size =
          Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.packed_policy_size (fun f ->
               Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
         ; Aws.Util.option_map v.assumed_role_user (fun f ->
               Aws.Query.Pair ("AssumedRoleUser", AssumedRoleUser.to_query f))
         ; Aws.Util.option_map v.credentials (fun f ->
               Aws.Query.Pair ("Credentials", Credentials.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.packed_policy_size (fun f ->
               "PackedPolicySize", Integer.to_json f)
         ; Aws.Util.option_map v.assumed_role_user (fun f ->
               "AssumedRoleUser", AssumedRoleUser.to_json f)
         ; Aws.Util.option_map v.credentials (fun f ->
               "Credentials", Credentials.to_json f)
         ])

  let of_json j =
    { credentials =
        Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
    ; assumed_role_user =
        Aws.Util.option_map (Aws.Json.lookup j "AssumedRoleUser") AssumedRoleUser.of_json
    ; packed_policy_size =
        Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
    }
end

module DecodeAuthorizationMessageResponse = struct
  type t = { decoded_message : String.t option }

  let make ?decoded_message () = { decoded_message }

  let parse xml =
    Some
      { decoded_message =
          Aws.Util.option_bind (Aws.Xml.member "DecodedMessage" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.decoded_message (fun f ->
               Aws.Query.Pair ("DecodedMessage", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.decoded_message (fun f ->
               "DecodedMessage", String.to_json f)
         ])

  let of_json j =
    { decoded_message =
        Aws.Util.option_map (Aws.Json.lookup j "DecodedMessage") String.of_json
    }
end

module ExpiredTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetCallerIdentityRequest = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module IDPCommunicationErrorException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module FederatedUser = struct
  type t =
    { federated_user_id : String.t
    ; arn : String.t
    }

  let make ~federated_user_id ~arn () = { federated_user_id; arn }

  let parse xml =
    Some
      { federated_user_id =
          Aws.Xml.required
            "FederatedUserId"
            (Aws.Util.option_bind (Aws.Xml.member "FederatedUserId" xml) String.parse)
      ; arn =
          Aws.Xml.required
            "Arn"
            (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
         ; Some (Aws.Query.Pair ("FederatedUserId", String.to_query v.federated_user_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Arn", String.to_json v.arn)
         ; Some ("FederatedUserId", String.to_json v.federated_user_id)
         ])

  let of_json j =
    { federated_user_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FederatedUserId"))
    ; arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
    }
end

module RegionDisabledException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module MalformedPolicyDocumentException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module AssumeRoleWithSAMLResponse = struct
  type t =
    { credentials : Credentials.t option
    ; assumed_role_user : AssumedRoleUser.t option
    ; packed_policy_size : Integer.t option
    ; subject : String.t option
    ; subject_type : String.t option
    ; issuer : String.t option
    ; audience : String.t option
    ; name_qualifier : String.t option
    }

  let make
      ?credentials
      ?assumed_role_user
      ?packed_policy_size
      ?subject
      ?subject_type
      ?issuer
      ?audience
      ?name_qualifier
      () =
    { credentials
    ; assumed_role_user
    ; packed_policy_size
    ; subject
    ; subject_type
    ; issuer
    ; audience
    ; name_qualifier
    }

  let parse xml =
    Some
      { credentials =
          Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
      ; assumed_role_user =
          Aws.Util.option_bind
            (Aws.Xml.member "AssumedRoleUser" xml)
            AssumedRoleUser.parse
      ; packed_policy_size =
          Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
      ; subject = Aws.Util.option_bind (Aws.Xml.member "Subject" xml) String.parse
      ; subject_type =
          Aws.Util.option_bind (Aws.Xml.member "SubjectType" xml) String.parse
      ; issuer = Aws.Util.option_bind (Aws.Xml.member "Issuer" xml) String.parse
      ; audience = Aws.Util.option_bind (Aws.Xml.member "Audience" xml) String.parse
      ; name_qualifier =
          Aws.Util.option_bind (Aws.Xml.member "NameQualifier" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name_qualifier (fun f ->
               Aws.Query.Pair ("NameQualifier", String.to_query f))
         ; Aws.Util.option_map v.audience (fun f ->
               Aws.Query.Pair ("Audience", String.to_query f))
         ; Aws.Util.option_map v.issuer (fun f ->
               Aws.Query.Pair ("Issuer", String.to_query f))
         ; Aws.Util.option_map v.subject_type (fun f ->
               Aws.Query.Pair ("SubjectType", String.to_query f))
         ; Aws.Util.option_map v.subject (fun f ->
               Aws.Query.Pair ("Subject", String.to_query f))
         ; Aws.Util.option_map v.packed_policy_size (fun f ->
               Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
         ; Aws.Util.option_map v.assumed_role_user (fun f ->
               Aws.Query.Pair ("AssumedRoleUser", AssumedRoleUser.to_query f))
         ; Aws.Util.option_map v.credentials (fun f ->
               Aws.Query.Pair ("Credentials", Credentials.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name_qualifier (fun f ->
               "NameQualifier", String.to_json f)
         ; Aws.Util.option_map v.audience (fun f -> "Audience", String.to_json f)
         ; Aws.Util.option_map v.issuer (fun f -> "Issuer", String.to_json f)
         ; Aws.Util.option_map v.subject_type (fun f -> "SubjectType", String.to_json f)
         ; Aws.Util.option_map v.subject (fun f -> "Subject", String.to_json f)
         ; Aws.Util.option_map v.packed_policy_size (fun f ->
               "PackedPolicySize", Integer.to_json f)
         ; Aws.Util.option_map v.assumed_role_user (fun f ->
               "AssumedRoleUser", AssumedRoleUser.to_json f)
         ; Aws.Util.option_map v.credentials (fun f ->
               "Credentials", Credentials.to_json f)
         ])

  let of_json j =
    { credentials =
        Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
    ; assumed_role_user =
        Aws.Util.option_map (Aws.Json.lookup j "AssumedRoleUser") AssumedRoleUser.of_json
    ; packed_policy_size =
        Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
    ; subject = Aws.Util.option_map (Aws.Json.lookup j "Subject") String.of_json
    ; subject_type = Aws.Util.option_map (Aws.Json.lookup j "SubjectType") String.of_json
    ; issuer = Aws.Util.option_map (Aws.Json.lookup j "Issuer") String.of_json
    ; audience = Aws.Util.option_map (Aws.Json.lookup j "Audience") String.of_json
    ; name_qualifier =
        Aws.Util.option_map (Aws.Json.lookup j "NameQualifier") String.of_json
    }
end

module IDPRejectedClaimException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetFederationTokenRequest = struct
  type t =
    { name : String.t
    ; policy : String.t option
    ; policy_arns : PolicyDescriptorListType.t
    ; duration_seconds : Integer.t option
    ; tags : TagListType.t
    }

  let make ~name ?policy ?(policy_arns = []) ?duration_seconds ?(tags = []) () =
    { name; policy; policy_arns; duration_seconds; tags }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
      ; policy_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyArns" xml)
               PolicyDescriptorListType.parse)
      ; duration_seconds =
          Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagListType.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagListType.to_query v.tags))
         ; Aws.Util.option_map v.duration_seconds (fun f ->
               Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ("PolicyArns.member", PolicyDescriptorListType.to_query v.policy_arns))
         ; Aws.Util.option_map v.policy (fun f ->
               Aws.Query.Pair ("Policy", String.to_query f))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagListType.to_json v.tags)
         ; Aws.Util.option_map v.duration_seconds (fun f ->
               "DurationSeconds", Integer.to_json f)
         ; Some ("PolicyArns", PolicyDescriptorListType.to_json v.policy_arns)
         ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
    ; policy_arns =
        PolicyDescriptorListType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyArns"))
    ; duration_seconds =
        Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
    ; tags = TagListType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module GetAccessKeyInfoRequest = struct
  type t = { access_key_id : String.t }

  let make ~access_key_id () = { access_key_id }

  let parse xml =
    Some
      { access_key_id =
          Aws.Xml.required
            "AccessKeyId"
            (Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("AccessKeyId", String.to_query v.access_key_id)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("AccessKeyId", String.to_json v.access_key_id) ])

  let of_json j =
    { access_key_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessKeyId"))
    }
end

module InvalidIdentityTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetFederationTokenResponse = struct
  type t =
    { credentials : Credentials.t option
    ; federated_user : FederatedUser.t option
    ; packed_policy_size : Integer.t option
    }

  let make ?credentials ?federated_user ?packed_policy_size () =
    { credentials; federated_user; packed_policy_size }

  let parse xml =
    Some
      { credentials =
          Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
      ; federated_user =
          Aws.Util.option_bind (Aws.Xml.member "FederatedUser" xml) FederatedUser.parse
      ; packed_policy_size =
          Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.packed_policy_size (fun f ->
               Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
         ; Aws.Util.option_map v.federated_user (fun f ->
               Aws.Query.Pair ("FederatedUser", FederatedUser.to_query f))
         ; Aws.Util.option_map v.credentials (fun f ->
               Aws.Query.Pair ("Credentials", Credentials.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.packed_policy_size (fun f ->
               "PackedPolicySize", Integer.to_json f)
         ; Aws.Util.option_map v.federated_user (fun f ->
               "FederatedUser", FederatedUser.to_json f)
         ; Aws.Util.option_map v.credentials (fun f ->
               "Credentials", Credentials.to_json f)
         ])

  let of_json j =
    { credentials =
        Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
    ; federated_user =
        Aws.Util.option_map (Aws.Json.lookup j "FederatedUser") FederatedUser.of_json
    ; packed_policy_size =
        Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
    }
end

module PackedPolicyTooLargeException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end
