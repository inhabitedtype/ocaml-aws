open Aws.BaseTypes

type t =
  { accepter_peering_connection_options : PeeringConnectionOptionsRequest.t option
  ; dry_run : Boolean.t option
  ; requester_peering_connection_options : PeeringConnectionOptionsRequest.t option
  ; vpc_peering_connection_id : String.t
  }

let make
    ?accepter_peering_connection_options
    ?dry_run
    ?requester_peering_connection_options
    ~vpc_peering_connection_id
    () =
  { accepter_peering_connection_options
  ; dry_run
  ; requester_peering_connection_options
  ; vpc_peering_connection_id
  }

let parse xml =
  Some
    { accepter_peering_connection_options =
        Aws.Util.option_bind
          (Aws.Xml.member "AccepterPeeringConnectionOptions" xml)
          PeeringConnectionOptionsRequest.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; requester_peering_connection_options =
        Aws.Util.option_bind
          (Aws.Xml.member "RequesterPeeringConnectionOptions" xml)
          PeeringConnectionOptionsRequest.parse
    ; vpc_peering_connection_id =
        Aws.Xml.required
          "VpcPeeringConnectionId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcPeeringConnectionId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("VpcPeeringConnectionId", String.to_query v.vpc_peering_connection_id))
       ; Aws.Util.option_map v.requester_peering_connection_options (fun f ->
             Aws.Query.Pair
               ( "RequesterPeeringConnectionOptions"
               , PeeringConnectionOptionsRequest.to_query f ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.accepter_peering_connection_options (fun f ->
             Aws.Query.Pair
               ( "AccepterPeeringConnectionOptions"
               , PeeringConnectionOptionsRequest.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VpcPeeringConnectionId", String.to_json v.vpc_peering_connection_id)
       ; Aws.Util.option_map v.requester_peering_connection_options (fun f ->
             ( "RequesterPeeringConnectionOptions"
             , PeeringConnectionOptionsRequest.to_json f ))
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.accepter_peering_connection_options (fun f ->
             "AccepterPeeringConnectionOptions", PeeringConnectionOptionsRequest.to_json f)
       ])

let of_json j =
  { accepter_peering_connection_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "AccepterPeeringConnectionOptions")
        PeeringConnectionOptionsRequest.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; requester_peering_connection_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "RequesterPeeringConnectionOptions")
        PeeringConnectionOptionsRequest.of_json
  ; vpc_peering_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcPeeringConnectionId"))
  }
