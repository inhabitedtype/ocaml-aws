open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; dry_run : Boolean.t option
  ; instance_id : String.t
  ; mac_credentials : String.t option
  ; mac_system_integrity_protection_configuration :
      MacSystemIntegrityProtectionConfigurationRequest.t option
  ; mac_system_integrity_protection_status : MacSystemIntegrityProtectionSettingStatus.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?client_token
    ?dry_run
    ~instance_id
    ?mac_credentials
    ?mac_system_integrity_protection_configuration
    ~mac_system_integrity_protection_status
    ?(tag_specifications = [])
    () =
  { client_token
  ; dry_run
  ; instance_id
  ; mac_credentials
  ; mac_system_integrity_protection_configuration
  ; mac_system_integrity_protection_status
  ; tag_specifications
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; mac_credentials =
        Aws.Util.option_bind (Aws.Xml.member "MacCredentials" xml) String.parse
    ; mac_system_integrity_protection_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "MacSystemIntegrityProtectionConfiguration" xml)
          MacSystemIntegrityProtectionConfigurationRequest.parse
    ; mac_system_integrity_protection_status =
        Aws.Xml.required
          "MacSystemIntegrityProtectionStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "MacSystemIntegrityProtectionStatus" xml)
             MacSystemIntegrityProtectionSettingStatus.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "MacSystemIntegrityProtectionStatus"
              , MacSystemIntegrityProtectionSettingStatus.to_query
                  v.mac_system_integrity_protection_status ))
       ; Aws.Util.option_map v.mac_system_integrity_protection_configuration (fun f ->
             Aws.Query.Pair
               ( "MacSystemIntegrityProtectionConfiguration"
               , MacSystemIntegrityProtectionConfigurationRequest.to_query f ))
       ; Aws.Util.option_map v.mac_credentials (fun f ->
             Aws.Query.Pair ("MacCredentials", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "MacSystemIntegrityProtectionStatus"
           , MacSystemIntegrityProtectionSettingStatus.to_json
               v.mac_system_integrity_protection_status )
       ; Aws.Util.option_map v.mac_system_integrity_protection_configuration (fun f ->
             ( "MacSystemIntegrityProtectionConfiguration"
             , MacSystemIntegrityProtectionConfigurationRequest.to_json f ))
       ; Aws.Util.option_map v.mac_credentials (fun f ->
             "MacCredentials", String.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; mac_credentials =
      Aws.Util.option_map (Aws.Json.lookup j "MacCredentials") String.of_json
  ; mac_system_integrity_protection_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "MacSystemIntegrityProtectionConfiguration")
        MacSystemIntegrityProtectionConfigurationRequest.of_json
  ; mac_system_integrity_protection_status =
      MacSystemIntegrityProtectionSettingStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MacSystemIntegrityProtectionStatus"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
