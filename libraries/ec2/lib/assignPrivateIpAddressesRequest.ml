open Aws.BaseTypes

type t =
  { ipv4_prefixes : IpPrefixList.t
  ; ipv4_prefix_count : Integer.t option
  ; network_interface_id : String.t
  ; private_ip_addresses : PrivateIpAddressStringList.t
  ; secondary_private_ip_address_count : Integer.t option
  ; allow_reassignment : Boolean.t option
  }

let make
    ?(ipv4_prefixes = [])
    ?ipv4_prefix_count
    ~network_interface_id
    ?(private_ip_addresses = [])
    ?secondary_private_ip_address_count
    ?allow_reassignment
    () =
  { ipv4_prefixes
  ; ipv4_prefix_count
  ; network_interface_id
  ; private_ip_addresses
  ; secondary_private_ip_address_count
  ; allow_reassignment
  }

let parse xml =
  Some
    { ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Ipv4Prefix" xml) IpPrefixList.parse)
    ; ipv4_prefix_count =
        Aws.Util.option_bind (Aws.Xml.member "Ipv4PrefixCount" xml) Integer.parse
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
    ; secondary_private_ip_address_count =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryPrivateIpAddressCount" xml)
          Integer.parse
    ; allow_reassignment =
        Aws.Util.option_bind (Aws.Xml.member "allowReassignment" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allow_reassignment (fun f ->
             Aws.Query.Pair ("AllowReassignment", Boolean.to_query f))
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             Aws.Query.Pair ("SecondaryPrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddress"
              , PrivateIpAddressStringList.to_query v.private_ip_addresses ))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Aws.Util.option_map v.ipv4_prefix_count (fun f ->
             Aws.Query.Pair ("Ipv4PrefixCount", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Ipv4Prefix", IpPrefixList.to_query v.ipv4_prefixes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allow_reassignment (fun f ->
             "allowReassignment", Boolean.to_json f)
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             "secondaryPrivateIpAddressCount", Integer.to_json f)
       ; Some
           ("privateIpAddress", PrivateIpAddressStringList.to_json v.private_ip_addresses)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Aws.Util.option_map v.ipv4_prefix_count (fun f ->
             "Ipv4PrefixCount", Integer.to_json f)
       ; Some ("Ipv4Prefix", IpPrefixList.to_json v.ipv4_prefixes)
       ])

let of_json j =
  { ipv4_prefixes =
      IpPrefixList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv4Prefix"))
  ; ipv4_prefix_count =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv4PrefixCount") Integer.of_json
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; private_ip_addresses =
      PrivateIpAddressStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddress"))
  ; secondary_private_ip_address_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "secondaryPrivateIpAddressCount")
        Integer.of_json
  ; allow_reassignment =
      Aws.Util.option_map (Aws.Json.lookup j "allowReassignment") Boolean.of_json
  }
