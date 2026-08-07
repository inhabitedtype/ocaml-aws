type t =
  { packet_header_statement : PacketHeaderStatementRequest.t option
  ; resource_statement : ResourceStatementRequest.t option
  }

let make ?packet_header_statement ?resource_statement () =
  { packet_header_statement; resource_statement }

let parse xml =
  Some
    { packet_header_statement =
        Aws.Util.option_bind
          (Aws.Xml.member "PacketHeaderStatement" xml)
          PacketHeaderStatementRequest.parse
    ; resource_statement =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceStatement" xml)
          ResourceStatementRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_statement (fun f ->
             Aws.Query.Pair ("ResourceStatement", ResourceStatementRequest.to_query f))
       ; Aws.Util.option_map v.packet_header_statement (fun f ->
             Aws.Query.Pair
               ("PacketHeaderStatement", PacketHeaderStatementRequest.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_statement (fun f ->
             "ResourceStatement", ResourceStatementRequest.to_json f)
       ; Aws.Util.option_map v.packet_header_statement (fun f ->
             "PacketHeaderStatement", PacketHeaderStatementRequest.to_json f)
       ])

let of_json j =
  { packet_header_statement =
      Aws.Util.option_map
        (Aws.Json.lookup j "PacketHeaderStatement")
        PacketHeaderStatementRequest.of_json
  ; resource_statement =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceStatement")
        ResourceStatementRequest.of_json
  }
