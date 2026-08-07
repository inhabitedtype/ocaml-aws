open Aws.BaseTypes

type t =
  { network_interface_id : String.t option
  ; assigned_private_ip_addresses : AssignedPrivateIpAddressList.t
  ; assigned_ipv4_prefixes : Ipv4PrefixesList.t
  }

let make
    ?network_interface_id
    ?(assigned_private_ip_addresses = [])
    ?(assigned_ipv4_prefixes = [])
    () =
  { network_interface_id; assigned_private_ip_addresses; assigned_ipv4_prefixes }

let parse xml =
  Some
    { network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; assigned_private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "assignedPrivateIpAddressesSet" xml)
             AssignedPrivateIpAddressList.parse)
    ; assigned_ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "assignedIpv4PrefixSet" xml)
             Ipv4PrefixesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AssignedIpv4PrefixSet", Ipv4PrefixesList.to_query v.assigned_ipv4_prefixes))
       ; Some
           (Aws.Query.Pair
              ( "AssignedPrivateIpAddressesSet"
              , AssignedPrivateIpAddressList.to_query v.assigned_private_ip_addresses ))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("assignedIpv4PrefixSet", Ipv4PrefixesList.to_json v.assigned_ipv4_prefixes)
       ; Some
           ( "assignedPrivateIpAddressesSet"
           , AssignedPrivateIpAddressList.to_json v.assigned_private_ip_addresses )
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ])

let of_json j =
  { network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; assigned_private_ip_addresses =
      AssignedPrivateIpAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "assignedPrivateIpAddressesSet"))
  ; assigned_ipv4_prefixes =
      Ipv4PrefixesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "assignedIpv4PrefixSet"))
  }
