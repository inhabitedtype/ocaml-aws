open Aws.BaseTypes

type t =
  { network_interface_id : String.t option
  ; unassigned_ipv6_addresses : Ipv6AddressList.t
  ; unassigned_ipv6_prefixes : IpPrefixList.t
  }

let make
    ?network_interface_id
    ?(unassigned_ipv6_addresses = [])
    ?(unassigned_ipv6_prefixes = [])
    () =
  { network_interface_id; unassigned_ipv6_addresses; unassigned_ipv6_prefixes }

let parse xml =
  Some
    { network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; unassigned_ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unassignedIpv6Addresses" xml)
             Ipv6AddressList.parse)
    ; unassigned_ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unassignedIpv6PrefixSet" xml)
             IpPrefixList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("UnassignedIpv6PrefixSet", IpPrefixList.to_query v.unassigned_ipv6_prefixes))
       ; Some
           (Aws.Query.Pair
              ( "UnassignedIpv6Addresses"
              , Ipv6AddressList.to_query v.unassigned_ipv6_addresses ))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("unassignedIpv6PrefixSet", IpPrefixList.to_json v.unassigned_ipv6_prefixes)
       ; Some
           ("unassignedIpv6Addresses", Ipv6AddressList.to_json v.unassigned_ipv6_addresses)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ])

let of_json j =
  { network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; unassigned_ipv6_addresses =
      Ipv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unassignedIpv6Addresses"))
  ; unassigned_ipv6_prefixes =
      IpPrefixList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unassignedIpv6PrefixSet"))
  }
