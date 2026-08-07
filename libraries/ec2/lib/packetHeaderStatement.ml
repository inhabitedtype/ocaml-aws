type t =
  { source_addresses : ValueStringList.t
  ; destination_addresses : ValueStringList.t
  ; source_ports : ValueStringList.t
  ; destination_ports : ValueStringList.t
  ; source_prefix_lists : ValueStringList.t
  ; destination_prefix_lists : ValueStringList.t
  ; protocols : ProtocolList.t
  }

let make
    ?(source_addresses = [])
    ?(destination_addresses = [])
    ?(source_ports = [])
    ?(destination_ports = [])
    ?(source_prefix_lists = [])
    ?(destination_prefix_lists = [])
    ?(protocols = [])
    () =
  { source_addresses
  ; destination_addresses
  ; source_ports
  ; destination_ports
  ; source_prefix_lists
  ; destination_prefix_lists
  ; protocols
  }

let parse xml =
  Some
    { source_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "sourceAddressSet" xml)
             ValueStringList.parse)
    ; destination_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationAddressSet" xml)
             ValueStringList.parse)
    ; source_ports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "sourcePortSet" xml)
             ValueStringList.parse)
    ; destination_ports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationPortSet" xml)
             ValueStringList.parse)
    ; source_prefix_lists =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "sourcePrefixListSet" xml)
             ValueStringList.parse)
    ; destination_prefix_lists =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationPrefixListSet" xml)
             ValueStringList.parse)
    ; protocols =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "protocolSet" xml) ProtocolList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ProtocolSet", ProtocolList.to_query v.protocols))
       ; Some
           (Aws.Query.Pair
              ( "DestinationPrefixListSet"
              , ValueStringList.to_query v.destination_prefix_lists ))
       ; Some
           (Aws.Query.Pair
              ("SourcePrefixListSet", ValueStringList.to_query v.source_prefix_lists))
       ; Some
           (Aws.Query.Pair
              ("DestinationPortSet", ValueStringList.to_query v.destination_ports))
       ; Some (Aws.Query.Pair ("SourcePortSet", ValueStringList.to_query v.source_ports))
       ; Some
           (Aws.Query.Pair
              ("DestinationAddressSet", ValueStringList.to_query v.destination_addresses))
       ; Some
           (Aws.Query.Pair
              ("SourceAddressSet", ValueStringList.to_query v.source_addresses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("protocolSet", ProtocolList.to_json v.protocols)
       ; Some
           ("destinationPrefixListSet", ValueStringList.to_json v.destination_prefix_lists)
       ; Some ("sourcePrefixListSet", ValueStringList.to_json v.source_prefix_lists)
       ; Some ("destinationPortSet", ValueStringList.to_json v.destination_ports)
       ; Some ("sourcePortSet", ValueStringList.to_json v.source_ports)
       ; Some ("destinationAddressSet", ValueStringList.to_json v.destination_addresses)
       ; Some ("sourceAddressSet", ValueStringList.to_json v.source_addresses)
       ])

let of_json j =
  { source_addresses =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "sourceAddressSet"))
  ; destination_addresses =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationAddressSet"))
  ; source_ports =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "sourcePortSet"))
  ; destination_ports =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationPortSet"))
  ; source_prefix_lists =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "sourcePrefixListSet"))
  ; destination_prefix_lists =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationPrefixListSet"))
  ; protocols =
      ProtocolList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "protocolSet"))
  }
