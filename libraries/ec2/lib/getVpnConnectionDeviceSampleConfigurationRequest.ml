open Aws.BaseTypes

type t =
  { vpn_connection_id : String.t
  ; vpn_connection_device_type_id : String.t
  ; internet_key_exchange_version : String.t option
  ; sample_type : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ~vpn_connection_id
    ~vpn_connection_device_type_id
    ?internet_key_exchange_version
    ?sample_type
    ?dry_run
    () =
  { vpn_connection_id
  ; vpn_connection_device_type_id
  ; internet_key_exchange_version
  ; sample_type
  ; dry_run
  }

let parse xml =
  Some
    { vpn_connection_id =
        Aws.Xml.required
          "VpnConnectionId"
          (Aws.Util.option_bind (Aws.Xml.member "VpnConnectionId" xml) String.parse)
    ; vpn_connection_device_type_id =
        Aws.Xml.required
          "VpnConnectionDeviceTypeId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VpnConnectionDeviceTypeId" xml)
             String.parse)
    ; internet_key_exchange_version =
        Aws.Util.option_bind
          (Aws.Xml.member "InternetKeyExchangeVersion" xml)
          String.parse
    ; sample_type = Aws.Util.option_bind (Aws.Xml.member "SampleType" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.sample_type (fun f ->
             Aws.Query.Pair ("SampleType", String.to_query f))
       ; Aws.Util.option_map v.internet_key_exchange_version (fun f ->
             Aws.Query.Pair ("InternetKeyExchangeVersion", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpnConnectionDeviceTypeId"
              , String.to_query v.vpn_connection_device_type_id ))
       ; Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.sample_type (fun f -> "SampleType", String.to_json f)
       ; Aws.Util.option_map v.internet_key_exchange_version (fun f ->
             "InternetKeyExchangeVersion", String.to_json f)
       ; Some ("VpnConnectionDeviceTypeId", String.to_json v.vpn_connection_device_type_id)
       ; Some ("VpnConnectionId", String.to_json v.vpn_connection_id)
       ])

let of_json j =
  { vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionId"))
  ; vpn_connection_device_type_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionDeviceTypeId"))
  ; internet_key_exchange_version =
      Aws.Util.option_map (Aws.Json.lookup j "InternetKeyExchangeVersion") String.of_json
  ; sample_type = Aws.Util.option_map (Aws.Json.lookup j "SampleType") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
