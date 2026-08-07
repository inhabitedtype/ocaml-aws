open Aws.BaseTypes

type t =
  { vpn_connection_device_types : VpnConnectionDeviceTypeList.t
  ; next_token : String.t option
  }

let make ?(vpn_connection_device_types = []) ?next_token () =
  { vpn_connection_device_types; next_token }

let parse xml =
  Some
    { vpn_connection_device_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpnConnectionDeviceTypeSet" xml)
             VpnConnectionDeviceTypeList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpnConnectionDeviceTypeSet"
              , VpnConnectionDeviceTypeList.to_query v.vpn_connection_device_types ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "vpnConnectionDeviceTypeSet"
           , VpnConnectionDeviceTypeList.to_json v.vpn_connection_device_types )
       ])

let of_json j =
  { vpn_connection_device_types =
      VpnConnectionDeviceTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpnConnectionDeviceTypeSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
