type t =
  { accepter_peering_connection_options : PeeringConnectionOptions.t option
  ; requester_peering_connection_options : PeeringConnectionOptions.t option
  }

let make ?accepter_peering_connection_options ?requester_peering_connection_options () =
  { accepter_peering_connection_options; requester_peering_connection_options }

let parse xml =
  Some
    { accepter_peering_connection_options =
        Aws.Util.option_bind
          (Aws.Xml.member "accepterPeeringConnectionOptions" xml)
          PeeringConnectionOptions.parse
    ; requester_peering_connection_options =
        Aws.Util.option_bind
          (Aws.Xml.member "requesterPeeringConnectionOptions" xml)
          PeeringConnectionOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.requester_peering_connection_options (fun f ->
             Aws.Query.Pair
               ("RequesterPeeringConnectionOptions", PeeringConnectionOptions.to_query f))
       ; Aws.Util.option_map v.accepter_peering_connection_options (fun f ->
             Aws.Query.Pair
               ("AccepterPeeringConnectionOptions", PeeringConnectionOptions.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.requester_peering_connection_options (fun f ->
             "requesterPeeringConnectionOptions", PeeringConnectionOptions.to_json f)
       ; Aws.Util.option_map v.accepter_peering_connection_options (fun f ->
             "accepterPeeringConnectionOptions", PeeringConnectionOptions.to_json f)
       ])

let of_json j =
  { accepter_peering_connection_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "accepterPeeringConnectionOptions")
        PeeringConnectionOptions.of_json
  ; requester_peering_connection_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "requesterPeeringConnectionOptions")
        PeeringConnectionOptions.of_json
  }
