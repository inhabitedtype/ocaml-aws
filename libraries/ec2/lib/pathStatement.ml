type t =
  { packet_header_statement : PacketHeaderStatement.t option
  ; resource_statement : ResourceStatement.t option
  }

let make ?packet_header_statement ?resource_statement () =
  { packet_header_statement; resource_statement }

let parse xml =
  Some
    { packet_header_statement =
        Aws.Util.option_bind
          (Aws.Xml.member "packetHeaderStatement" xml)
          PacketHeaderStatement.parse
    ; resource_statement =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceStatement" xml)
          ResourceStatement.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_statement (fun f ->
             Aws.Query.Pair ("ResourceStatement", ResourceStatement.to_query f))
       ; Aws.Util.option_map v.packet_header_statement (fun f ->
             Aws.Query.Pair ("PacketHeaderStatement", PacketHeaderStatement.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_statement (fun f ->
             "resourceStatement", ResourceStatement.to_json f)
       ; Aws.Util.option_map v.packet_header_statement (fun f ->
             "packetHeaderStatement", PacketHeaderStatement.to_json f)
       ])

let of_json j =
  { packet_header_statement =
      Aws.Util.option_map
        (Aws.Json.lookup j "packetHeaderStatement")
        PacketHeaderStatement.of_json
  ; resource_statement =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceStatement")
        ResourceStatement.of_json
  }
