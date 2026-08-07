open Aws.BaseTypes

type t =
  { verified_access_trust_provider_id : String.t option
  ; description : String.t option
  ; trust_provider_type : TrustProviderType.t option
  ; user_trust_provider_type : UserTrustProviderType.t option
  ; device_trust_provider_type : DeviceTrustProviderType.t option
  ; oidc_options : OidcOptions.t option
  ; device_options : DeviceOptions.t option
  ; policy_reference_name : String.t option
  ; creation_time : String.t option
  ; last_updated_time : String.t option
  ; tags : TagList.t
  ; sse_specification : VerifiedAccessSseSpecificationResponse.t option
  ; native_application_oidc_options : NativeApplicationOidcOptions.t option
  }

let make
    ?verified_access_trust_provider_id
    ?description
    ?trust_provider_type
    ?user_trust_provider_type
    ?device_trust_provider_type
    ?oidc_options
    ?device_options
    ?policy_reference_name
    ?creation_time
    ?last_updated_time
    ?(tags = [])
    ?sse_specification
    ?native_application_oidc_options
    () =
  { verified_access_trust_provider_id
  ; description
  ; trust_provider_type
  ; user_trust_provider_type
  ; device_trust_provider_type
  ; oidc_options
  ; device_options
  ; policy_reference_name
  ; creation_time
  ; last_updated_time
  ; tags
  ; sse_specification
  ; native_application_oidc_options
  }

let parse xml =
  Some
    { verified_access_trust_provider_id =
        Aws.Util.option_bind
          (Aws.Xml.member "verifiedAccessTrustProviderId" xml)
          String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; trust_provider_type =
        Aws.Util.option_bind
          (Aws.Xml.member "trustProviderType" xml)
          TrustProviderType.parse
    ; user_trust_provider_type =
        Aws.Util.option_bind
          (Aws.Xml.member "userTrustProviderType" xml)
          UserTrustProviderType.parse
    ; device_trust_provider_type =
        Aws.Util.option_bind
          (Aws.Xml.member "deviceTrustProviderType" xml)
          DeviceTrustProviderType.parse
    ; oidc_options =
        Aws.Util.option_bind (Aws.Xml.member "oidcOptions" xml) OidcOptions.parse
    ; device_options =
        Aws.Util.option_bind (Aws.Xml.member "deviceOptions" xml) DeviceOptions.parse
    ; policy_reference_name =
        Aws.Util.option_bind (Aws.Xml.member "policyReferenceName" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) String.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdatedTime" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "sseSpecification" xml)
          VerifiedAccessSseSpecificationResponse.parse
    ; native_application_oidc_options =
        Aws.Util.option_bind
          (Aws.Xml.member "nativeApplicationOidcOptions" xml)
          NativeApplicationOidcOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.native_application_oidc_options (fun f ->
             Aws.Query.Pair
               ("NativeApplicationOidcOptions", NativeApplicationOidcOptions.to_query f))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationResponse.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", String.to_query f))
       ; Aws.Util.option_map v.policy_reference_name (fun f ->
             Aws.Query.Pair ("PolicyReferenceName", String.to_query f))
       ; Aws.Util.option_map v.device_options (fun f ->
             Aws.Query.Pair ("DeviceOptions", DeviceOptions.to_query f))
       ; Aws.Util.option_map v.oidc_options (fun f ->
             Aws.Query.Pair ("OidcOptions", OidcOptions.to_query f))
       ; Aws.Util.option_map v.device_trust_provider_type (fun f ->
             Aws.Query.Pair ("DeviceTrustProviderType", DeviceTrustProviderType.to_query f))
       ; Aws.Util.option_map v.user_trust_provider_type (fun f ->
             Aws.Query.Pair ("UserTrustProviderType", UserTrustProviderType.to_query f))
       ; Aws.Util.option_map v.trust_provider_type (fun f ->
             Aws.Query.Pair ("TrustProviderType", TrustProviderType.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.verified_access_trust_provider_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessTrustProviderId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.native_application_oidc_options (fun f ->
             "nativeApplicationOidcOptions", NativeApplicationOidcOptions.to_json f)
       ; Aws.Util.option_map v.sse_specification (fun f ->
             "sseSpecification", VerifiedAccessSseSpecificationResponse.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "lastUpdatedTime", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", String.to_json f)
       ; Aws.Util.option_map v.policy_reference_name (fun f ->
             "policyReferenceName", String.to_json f)
       ; Aws.Util.option_map v.device_options (fun f ->
             "deviceOptions", DeviceOptions.to_json f)
       ; Aws.Util.option_map v.oidc_options (fun f ->
             "oidcOptions", OidcOptions.to_json f)
       ; Aws.Util.option_map v.device_trust_provider_type (fun f ->
             "deviceTrustProviderType", DeviceTrustProviderType.to_json f)
       ; Aws.Util.option_map v.user_trust_provider_type (fun f ->
             "userTrustProviderType", UserTrustProviderType.to_json f)
       ; Aws.Util.option_map v.trust_provider_type (fun f ->
             "trustProviderType", TrustProviderType.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.verified_access_trust_provider_id (fun f ->
             "verifiedAccessTrustProviderId", String.to_json f)
       ])

let of_json j =
  { verified_access_trust_provider_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "verifiedAccessTrustProviderId")
        String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; trust_provider_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "trustProviderType")
        TrustProviderType.of_json
  ; user_trust_provider_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "userTrustProviderType")
        UserTrustProviderType.of_json
  ; device_trust_provider_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "deviceTrustProviderType")
        DeviceTrustProviderType.of_json
  ; oidc_options =
      Aws.Util.option_map (Aws.Json.lookup j "oidcOptions") OidcOptions.of_json
  ; device_options =
      Aws.Util.option_map (Aws.Json.lookup j "deviceOptions") DeviceOptions.of_json
  ; policy_reference_name =
      Aws.Util.option_map (Aws.Json.lookup j "policyReferenceName") String.of_json
  ; creation_time = Aws.Util.option_map (Aws.Json.lookup j "creationTime") String.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdatedTime") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "sseSpecification")
        VerifiedAccessSseSpecificationResponse.of_json
  ; native_application_oidc_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "nativeApplicationOidcOptions")
        NativeApplicationOidcOptions.of_json
  }
