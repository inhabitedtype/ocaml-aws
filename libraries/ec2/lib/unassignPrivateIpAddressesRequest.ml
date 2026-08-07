open Aws.BaseTypes

type t =
  { ipv4_prefixes : IpPrefixList.t
  ; network_interface_id : String.t
  ; private_ip_addresses : PrivateIpAddressStringList.t
  }

let make ?(ipv4_prefixes = []) ~network_interface_id ?(private_ip_addresses = []) () =
  { ipv4_prefixes; network_interface_id; private_ip_addresses }

let parse xml =
  Some
    { ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Ipv4Prefix" xml) IpPrefixList.parse)
    ; network_interface_id =
        Aws.Xml.required
          "networkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse)
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddress" xml)
             PrivateIpAddressStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PrivateIpAddress"
              , PrivateIpAddressStringList.to_query v.private_ip_addresses ))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Some (Aws.Query.Pair ("Ipv4Prefix", IpPrefixList.to_query v.ipv4_prefixes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("privateIpAddress", PrivateIpAddressStringList.to_json v.private_ip_addresses)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Some ("Ipv4Prefix", IpPrefixList.to_json v.ipv4_prefixes)
       ])

let of_json j =
  { ipv4_prefixes =
      IpPrefixList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv4Prefix"))
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; private_ip_addresses =
      PrivateIpAddressStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddress"))
  }
