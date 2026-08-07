open Aws.BaseTypes

type t =
  { destination_cidr_block : String.t
  ; vpn_connection_id : String.t
  }

let make ~destination_cidr_block ~vpn_connection_id () =
  { destination_cidr_block; vpn_connection_id }

let parse xml =
  Some
    { destination_cidr_block =
        Aws.Xml.required
          "DestinationCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse)
    ; vpn_connection_id =
        Aws.Xml.required
          "VpnConnectionId"
          (Aws.Util.option_bind (Aws.Xml.member "VpnConnectionId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ; Some
           (Aws.Query.Pair
              ("DestinationCidrBlock", String.to_query v.destination_cidr_block))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VpnConnectionId", String.to_json v.vpn_connection_id)
       ; Some ("DestinationCidrBlock", String.to_json v.destination_cidr_block)
       ])

let of_json j =
  { destination_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationCidrBlock"))
  ; vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionId"))
  }
