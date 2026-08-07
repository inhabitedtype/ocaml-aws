open Aws.BaseTypes

type t =
  { verified_access_trust_provider_id : String.t option
  ; description : String.t option
  ; trust_provider_type : TrustProviderType.t option
  ; user_trust_provider_type : UserTrustProviderType.t option
  ; device_trust_provider_type : DeviceTrustProviderType.t option
  }

let make
    ?verified_access_trust_provider_id
    ?description
    ?trust_provider_type
    ?user_trust_provider_type
    ?device_trust_provider_type
    () =
  { verified_access_trust_provider_id
  ; description
  ; trust_provider_type
  ; user_trust_provider_type
  ; device_trust_provider_type
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
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.device_trust_provider_type (fun f ->
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
       [ Aws.Util.option_map v.device_trust_provider_type (fun f ->
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
  }
