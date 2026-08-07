open Aws.BaseTypes

type t = { vpn_connection_device_sample_configuration : String.t option }

let make ?vpn_connection_device_sample_configuration () =
  { vpn_connection_device_sample_configuration }

let parse xml =
  Some
    { vpn_connection_device_sample_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "vpnConnectionDeviceSampleConfiguration" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_connection_device_sample_configuration (fun f ->
             Aws.Query.Pair ("VpnConnectionDeviceSampleConfiguration", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_connection_device_sample_configuration (fun f ->
             "vpnConnectionDeviceSampleConfiguration", String.to_json f)
       ])

let of_json j =
  { vpn_connection_device_sample_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpnConnectionDeviceSampleConfiguration")
        String.of_json
  }
