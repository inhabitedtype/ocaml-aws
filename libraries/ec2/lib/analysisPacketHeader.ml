open Aws.BaseTypes

type t =
  { destination_addresses : IpAddressList.t
  ; destination_port_ranges : PortRangeList.t
  ; protocol : String.t option
  ; source_addresses : IpAddressList.t
  ; source_port_ranges : PortRangeList.t
  }

let make
    ?(destination_addresses = [])
    ?(destination_port_ranges = [])
    ?protocol
    ?(source_addresses = [])
    ?(source_port_ranges = [])
    () =
  { destination_addresses
  ; destination_port_ranges
  ; protocol
  ; source_addresses
  ; source_port_ranges
  }

let parse xml =
  Some
    { destination_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationAddressSet" xml)
             IpAddressList.parse)
    ; destination_port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationPortRangeSet" xml)
             PortRangeList.parse)
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    ; source_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "sourceAddressSet" xml)
             IpAddressList.parse)
    ; source_port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "sourcePortRangeSet" xml)
             PortRangeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SourcePortRangeSet", PortRangeList.to_query v.source_port_ranges))
       ; Some
           (Aws.Query.Pair ("SourceAddressSet", IpAddressList.to_query v.source_addresses))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DestinationPortRangeSet", PortRangeList.to_query v.destination_port_ranges))
       ; Some
           (Aws.Query.Pair
              ("DestinationAddressSet", IpAddressList.to_query v.destination_addresses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("sourcePortRangeSet", PortRangeList.to_json v.source_port_ranges)
       ; Some ("sourceAddressSet", IpAddressList.to_json v.source_addresses)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Some ("destinationPortRangeSet", PortRangeList.to_json v.destination_port_ranges)
       ; Some ("destinationAddressSet", IpAddressList.to_json v.destination_addresses)
       ])

let of_json j =
  { destination_addresses =
      IpAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationAddressSet"))
  ; destination_port_ranges =
      PortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationPortRangeSet"))
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  ; source_addresses =
      IpAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "sourceAddressSet"))
  ; source_port_ranges =
      PortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "sourcePortRangeSet"))
  }
