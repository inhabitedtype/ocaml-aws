open Aws.BaseTypes

type t =
  { trust_provider_type : TrustProviderType.t
  ; user_trust_provider_type : UserTrustProviderType.t option
  ; device_trust_provider_type : DeviceTrustProviderType.t option
  ; oidc_options : CreateVerifiedAccessTrustProviderOidcOptions.t option
  ; device_options : CreateVerifiedAccessTrustProviderDeviceOptions.t option
  ; policy_reference_name : String.t
  ; description : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; sse_specification : VerifiedAccessSseSpecificationRequest.t option
  ; native_application_oidc_options :
      CreateVerifiedAccessNativeApplicationOidcOptions.t option
  }

let make
    ~trust_provider_type
    ?user_trust_provider_type
    ?device_trust_provider_type
    ?oidc_options
    ?device_options
    ~policy_reference_name
    ?description
    ?(tag_specifications = [])
    ?client_token
    ?dry_run
    ?sse_specification
    ?native_application_oidc_options
    () =
  { trust_provider_type
  ; user_trust_provider_type
  ; device_trust_provider_type
  ; oidc_options
  ; device_options
  ; policy_reference_name
  ; description
  ; tag_specifications
  ; client_token
  ; dry_run
  ; sse_specification
  ; native_application_oidc_options
  }

let parse xml =
  Some
    { trust_provider_type =
        Aws.Xml.required
          "TrustProviderType"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrustProviderType" xml)
             TrustProviderType.parse)
    ; user_trust_provider_type =
        Aws.Util.option_bind
          (Aws.Xml.member "UserTrustProviderType" xml)
          UserTrustProviderType.parse
    ; device_trust_provider_type =
        Aws.Util.option_bind
          (Aws.Xml.member "DeviceTrustProviderType" xml)
          DeviceTrustProviderType.parse
    ; oidc_options =
        Aws.Util.option_bind
          (Aws.Xml.member "OidcOptions" xml)
          CreateVerifiedAccessTrustProviderOidcOptions.parse
    ; device_options =
        Aws.Util.option_bind
          (Aws.Xml.member "DeviceOptions" xml)
          CreateVerifiedAccessTrustProviderDeviceOptions.parse
    ; policy_reference_name =
        Aws.Xml.required
          "PolicyReferenceName"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyReferenceName" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "SseSpecification" xml)
          VerifiedAccessSseSpecificationRequest.parse
    ; native_application_oidc_options =
        Aws.Util.option_bind
          (Aws.Xml.member "NativeApplicationOidcOptions" xml)
          CreateVerifiedAccessNativeApplicationOidcOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.native_application_oidc_options (fun f ->
             Aws.Query.Pair
               ( "NativeApplicationOidcOptions"
               , CreateVerifiedAccessNativeApplicationOidcOptions.to_query f ))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair ("PolicyReferenceName", String.to_query v.policy_reference_name))
       ; Aws.Util.option_map v.device_options (fun f ->
             Aws.Query.Pair
               ("DeviceOptions", CreateVerifiedAccessTrustProviderDeviceOptions.to_query f))
       ; Aws.Util.option_map v.oidc_options (fun f ->
             Aws.Query.Pair
               ("OidcOptions", CreateVerifiedAccessTrustProviderOidcOptions.to_query f))
       ; Aws.Util.option_map v.device_trust_provider_type (fun f ->
             Aws.Query.Pair ("DeviceTrustProviderType", DeviceTrustProviderType.to_query f))
       ; Aws.Util.option_map v.user_trust_provider_type (fun f ->
             Aws.Query.Pair ("UserTrustProviderType", UserTrustProviderType.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrustProviderType", TrustProviderType.to_query v.trust_provider_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.native_application_oidc_options (fun f ->
             ( "NativeApplicationOidcOptions"
             , CreateVerifiedAccessNativeApplicationOidcOptions.to_json f ))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             "SseSpecification", VerifiedAccessSseSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("PolicyReferenceName", String.to_json v.policy_reference_name)
       ; Aws.Util.option_map v.device_options (fun f ->
             "DeviceOptions", CreateVerifiedAccessTrustProviderDeviceOptions.to_json f)
       ; Aws.Util.option_map v.oidc_options (fun f ->
             "OidcOptions", CreateVerifiedAccessTrustProviderOidcOptions.to_json f)
       ; Aws.Util.option_map v.device_trust_provider_type (fun f ->
             "DeviceTrustProviderType", DeviceTrustProviderType.to_json f)
       ; Aws.Util.option_map v.user_trust_provider_type (fun f ->
             "UserTrustProviderType", UserTrustProviderType.to_json f)
       ; Some ("TrustProviderType", TrustProviderType.to_json v.trust_provider_type)
       ])

let of_json j =
  { trust_provider_type =
      TrustProviderType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrustProviderType"))
  ; user_trust_provider_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "UserTrustProviderType")
        UserTrustProviderType.of_json
  ; device_trust_provider_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeviceTrustProviderType")
        DeviceTrustProviderType.of_json
  ; oidc_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "OidcOptions")
        CreateVerifiedAccessTrustProviderOidcOptions.of_json
  ; device_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeviceOptions")
        CreateVerifiedAccessTrustProviderDeviceOptions.of_json
  ; policy_reference_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyReferenceName"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "SseSpecification")
        VerifiedAccessSseSpecificationRequest.of_json
  ; native_application_oidc_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "NativeApplicationOidcOptions")
        CreateVerifiedAccessNativeApplicationOidcOptions.of_json
  }
