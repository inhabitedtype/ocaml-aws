open Aws.BaseTypes

type t =
  { vpn_connection_device_type_id : String.t option
  ; vendor : String.t option
  ; platform : String.t option
  ; software : String.t option
  }

let make ?vpn_connection_device_type_id ?vendor ?platform ?software () =
  { vpn_connection_device_type_id; vendor; platform; software }

let parse xml =
  Some
    { vpn_connection_device_type_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnConnectionDeviceTypeId" xml) String.parse
    ; vendor = Aws.Util.option_bind (Aws.Xml.member "vendor" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) String.parse
    ; software = Aws.Util.option_bind (Aws.Xml.member "software" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.software (fun f ->
             Aws.Query.Pair ("Software", String.to_query f))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", String.to_query f))
       ; Aws.Util.option_map v.vendor (fun f ->
             Aws.Query.Pair ("Vendor", String.to_query f))
       ; Aws.Util.option_map v.vpn_connection_device_type_id (fun f ->
             Aws.Query.Pair ("VpnConnectionDeviceTypeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.software (fun f -> "software", String.to_json f)
       ; Aws.Util.option_map v.platform (fun f -> "platform", String.to_json f)
       ; Aws.Util.option_map v.vendor (fun f -> "vendor", String.to_json f)
       ; Aws.Util.option_map v.vpn_connection_device_type_id (fun f ->
             "vpnConnectionDeviceTypeId", String.to_json f)
       ])

let of_json j =
  { vpn_connection_device_type_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConnectionDeviceTypeId") String.of_json
  ; vendor = Aws.Util.option_map (Aws.Json.lookup j "vendor") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") String.of_json
  ; software = Aws.Util.option_map (Aws.Json.lookup j "software") String.of_json
  }
