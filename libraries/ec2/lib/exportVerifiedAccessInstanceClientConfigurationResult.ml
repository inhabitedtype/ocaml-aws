open Aws.BaseTypes

type t =
  { version : String.t option
  ; verified_access_instance_id : String.t option
  ; region : String.t option
  ; device_trust_providers : DeviceTrustProviderTypeList.t
  ; user_trust_provider :
      VerifiedAccessInstanceUserTrustProviderClientConfiguration.t option
  ; open_vpn_configurations : VerifiedAccessInstanceOpenVpnClientConfigurationList.t
  }

let make
    ?version
    ?verified_access_instance_id
    ?region
    ?(device_trust_providers = [])
    ?user_trust_provider
    ?(open_vpn_configurations = [])
    () =
  { version
  ; verified_access_instance_id
  ; region
  ; device_trust_providers
  ; user_trust_provider
  ; open_vpn_configurations
  }

let parse xml =
  Some
    { version = Aws.Util.option_bind (Aws.Xml.member "version" xml) String.parse
    ; verified_access_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessInstanceId" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "region" xml) String.parse
    ; device_trust_providers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "deviceTrustProviderSet" xml)
             DeviceTrustProviderTypeList.parse)
    ; user_trust_provider =
        Aws.Util.option_bind
          (Aws.Xml.member "userTrustProvider" xml)
          VerifiedAccessInstanceUserTrustProviderClientConfiguration.parse
    ; open_vpn_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "openVpnConfigurationSet" xml)
             VerifiedAccessInstanceOpenVpnClientConfigurationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "OpenVpnConfigurationSet"
              , VerifiedAccessInstanceOpenVpnClientConfigurationList.to_query
                  v.open_vpn_configurations ))
       ; Aws.Util.option_map v.user_trust_provider (fun f ->
             Aws.Query.Pair
               ( "UserTrustProvider"
               , VerifiedAccessInstanceUserTrustProviderClientConfiguration.to_query f ))
       ; Some
           (Aws.Query.Pair
              ( "DeviceTrustProviderSet"
              , DeviceTrustProviderTypeList.to_query v.device_trust_providers ))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstanceId", String.to_query f))
       ; Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "openVpnConfigurationSet"
           , VerifiedAccessInstanceOpenVpnClientConfigurationList.to_json
               v.open_vpn_configurations )
       ; Aws.Util.option_map v.user_trust_provider (fun f ->
             ( "userTrustProvider"
             , VerifiedAccessInstanceUserTrustProviderClientConfiguration.to_json f ))
       ; Some
           ( "deviceTrustProviderSet"
           , DeviceTrustProviderTypeList.to_json v.device_trust_providers )
       ; Aws.Util.option_map v.region (fun f -> "region", String.to_json f)
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             "verifiedAccessInstanceId", String.to_json f)
       ; Aws.Util.option_map v.version (fun f -> "version", String.to_json f)
       ])

let of_json j =
  { version = Aws.Util.option_map (Aws.Json.lookup j "version") String.of_json
  ; verified_access_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessInstanceId") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "region") String.of_json
  ; device_trust_providers =
      DeviceTrustProviderTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "deviceTrustProviderSet"))
  ; user_trust_provider =
      Aws.Util.option_map
        (Aws.Json.lookup j "userTrustProvider")
        VerifiedAccessInstanceUserTrustProviderClientConfiguration.of_json
  ; open_vpn_configurations =
      VerifiedAccessInstanceOpenVpnClientConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "openVpnConfigurationSet"))
  }
