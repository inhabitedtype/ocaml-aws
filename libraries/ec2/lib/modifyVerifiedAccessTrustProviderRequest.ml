open Aws.BaseTypes

type t =
  { verified_access_trust_provider_id : String.t
  ; oidc_options : ModifyVerifiedAccessTrustProviderOidcOptions.t option
  ; device_options : ModifyVerifiedAccessTrustProviderDeviceOptions.t option
  ; description : String.t option
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  ; sse_specification : VerifiedAccessSseSpecificationRequest.t option
  ; native_application_oidc_options :
      ModifyVerifiedAccessNativeApplicationOidcOptions.t option
  }

let make
    ~verified_access_trust_provider_id
    ?oidc_options
    ?device_options
    ?description
    ?dry_run
    ?client_token
    ?sse_specification
    ?native_application_oidc_options
    () =
  { verified_access_trust_provider_id
  ; oidc_options
  ; device_options
  ; description
  ; dry_run
  ; client_token
  ; sse_specification
  ; native_application_oidc_options
  }

let parse xml =
  Some
    { verified_access_trust_provider_id =
        Aws.Xml.required
          "VerifiedAccessTrustProviderId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessTrustProviderId" xml)
             String.parse)
    ; oidc_options =
        Aws.Util.option_bind
          (Aws.Xml.member "OidcOptions" xml)
          ModifyVerifiedAccessTrustProviderOidcOptions.parse
    ; device_options =
        Aws.Util.option_bind
          (Aws.Xml.member "DeviceOptions" xml)
          ModifyVerifiedAccessTrustProviderDeviceOptions.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "SseSpecification" xml)
          VerifiedAccessSseSpecificationRequest.parse
    ; native_application_oidc_options =
        Aws.Util.option_bind
          (Aws.Xml.member "NativeApplicationOidcOptions" xml)
          ModifyVerifiedAccessNativeApplicationOidcOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.native_application_oidc_options (fun f ->
             Aws.Query.Pair
               ( "NativeApplicationOidcOptions"
               , ModifyVerifiedAccessNativeApplicationOidcOptions.to_query f ))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.device_options (fun f ->
             Aws.Query.Pair
               ("DeviceOptions", ModifyVerifiedAccessTrustProviderDeviceOptions.to_query f))
       ; Aws.Util.option_map v.oidc_options (fun f ->
             Aws.Query.Pair
               ("OidcOptions", ModifyVerifiedAccessTrustProviderOidcOptions.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VerifiedAccessTrustProviderId"
              , String.to_query v.verified_access_trust_provider_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.native_application_oidc_options (fun f ->
             ( "NativeApplicationOidcOptions"
             , ModifyVerifiedAccessNativeApplicationOidcOptions.to_json f ))
       ; Aws.Util.option_map v.sse_specification (fun f ->
             "SseSpecification", VerifiedAccessSseSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.device_options (fun f ->
             "DeviceOptions", ModifyVerifiedAccessTrustProviderDeviceOptions.to_json f)
       ; Aws.Util.option_map v.oidc_options (fun f ->
             "OidcOptions", ModifyVerifiedAccessTrustProviderOidcOptions.to_json f)
       ; Some
           ( "VerifiedAccessTrustProviderId"
           , String.to_json v.verified_access_trust_provider_id )
       ])

let of_json j =
  { verified_access_trust_provider_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessTrustProviderId"))
  ; oidc_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "OidcOptions")
        ModifyVerifiedAccessTrustProviderOidcOptions.of_json
  ; device_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeviceOptions")
        ModifyVerifiedAccessTrustProviderDeviceOptions.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "SseSpecification")
        VerifiedAccessSseSpecificationRequest.of_json
  ; native_application_oidc_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "NativeApplicationOidcOptions")
        ModifyVerifiedAccessNativeApplicationOidcOptions.of_json
  }
