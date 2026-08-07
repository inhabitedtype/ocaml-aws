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
             (Aws.Xml.member "SourceAddress" xml)
             ValueStringList.parse)
    ; destination_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DestinationAddress" xml)
             ValueStringList.parse)
    ; source_ports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SourcePort" xml) ValueStringList.parse)
    ; destination_ports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DestinationPort" xml)
             ValueStringList.parse)
    ; source_prefix_lists =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SourcePrefixList" xml)
             ValueStringList.parse)
    ; destination_prefix_lists =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DestinationPrefixList" xml)
             ValueStringList.parse)
    ; protocols =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) ProtocolList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Protocol", ProtocolList.to_query v.protocols))
       ; Some
           (Aws.Query.Pair
              ( "DestinationPrefixList"
              , ValueStringList.to_query v.destination_prefix_lists ))
       ; Some
           (Aws.Query.Pair
              ("SourcePrefixList", ValueStringList.to_query v.source_prefix_lists))
       ; Some
           (Aws.Query.Pair
              ("DestinationPort", ValueStringList.to_query v.destination_ports))
       ; Some (Aws.Query.Pair ("SourcePort", ValueStringList.to_query v.source_ports))
       ; Some
           (Aws.Query.Pair
              ("DestinationAddress", ValueStringList.to_query v.destination_addresses))
       ; Some
           (Aws.Query.Pair ("SourceAddress", ValueStringList.to_query v.source_addresses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Protocol", ProtocolList.to_json v.protocols)
       ; Some ("DestinationPrefixList", ValueStringList.to_json v.destination_prefix_lists)
       ; Some ("SourcePrefixList", ValueStringList.to_json v.source_prefix_lists)
       ; Some ("DestinationPort", ValueStringList.to_json v.destination_ports)
       ; Some ("SourcePort", ValueStringList.to_json v.source_ports)
       ; Some ("DestinationAddress", ValueStringList.to_json v.destination_addresses)
       ; Some ("SourceAddress", ValueStringList.to_json v.source_addresses)
       ])

let of_json j =
  { source_addresses =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceAddress"))
  ; destination_addresses =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationAddress"))
  ; source_ports =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourcePort"))
  ; destination_ports =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationPort"))
  ; source_prefix_lists =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourcePrefixList"))
  ; destination_prefix_lists =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationPrefixList"))
  ; protocols =
      ProtocolList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Protocol"))
  }
