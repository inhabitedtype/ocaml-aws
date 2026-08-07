open Aws.BaseTypes

type t =
  { network_insights_path_id : String.t option
  ; network_insights_path_arn : String.t option
  ; created_date : DateTime.t option
  ; source : String.t option
  ; destination : String.t option
  ; source_arn : String.t option
  ; destination_arn : String.t option
  ; source_ip : String.t option
  ; destination_ip : String.t option
  ; protocol : Protocol.t option
  ; destination_port : Integer.t option
  ; tags : TagList.t
  ; filter_at_source : PathFilter.t option
  ; filter_at_destination : PathFilter.t option
  }

let make
    ?network_insights_path_id
    ?network_insights_path_arn
    ?created_date
    ?source
    ?destination
    ?source_arn
    ?destination_arn
    ?source_ip
    ?destination_ip
    ?protocol
    ?destination_port
    ?(tags = [])
    ?filter_at_source
    ?filter_at_destination
    () =
  { network_insights_path_id
  ; network_insights_path_arn
  ; created_date
  ; source
  ; destination
  ; source_arn
  ; destination_arn
  ; source_ip
  ; destination_ip
  ; protocol
  ; destination_port
  ; tags
  ; filter_at_source
  ; filter_at_destination
  }

let parse xml =
  Some
    { network_insights_path_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInsightsPathId" xml) String.parse
    ; network_insights_path_arn =
        Aws.Util.option_bind (Aws.Xml.member "networkInsightsPathArn" xml) String.parse
    ; created_date =
        Aws.Util.option_bind (Aws.Xml.member "createdDate" xml) DateTime.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "source" xml) String.parse
    ; destination = Aws.Util.option_bind (Aws.Xml.member "destination" xml) String.parse
    ; source_arn = Aws.Util.option_bind (Aws.Xml.member "sourceArn" xml) String.parse
    ; destination_arn =
        Aws.Util.option_bind (Aws.Xml.member "destinationArn" xml) String.parse
    ; source_ip = Aws.Util.option_bind (Aws.Xml.member "sourceIp" xml) String.parse
    ; destination_ip =
        Aws.Util.option_bind (Aws.Xml.member "destinationIp" xml) String.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) Protocol.parse
    ; destination_port =
        Aws.Util.option_bind (Aws.Xml.member "destinationPort" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; filter_at_source =
        Aws.Util.option_bind (Aws.Xml.member "filterAtSource" xml) PathFilter.parse
    ; filter_at_destination =
        Aws.Util.option_bind (Aws.Xml.member "filterAtDestination" xml) PathFilter.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.filter_at_destination (fun f ->
             Aws.Query.Pair ("FilterAtDestination", PathFilter.to_query f))
       ; Aws.Util.option_map v.filter_at_source (fun f ->
             Aws.Query.Pair ("FilterAtSource", PathFilter.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.destination_port (fun f ->
             Aws.Query.Pair ("DestinationPort", Integer.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", Protocol.to_query f))
       ; Aws.Util.option_map v.destination_ip (fun f ->
             Aws.Query.Pair ("DestinationIp", String.to_query f))
       ; Aws.Util.option_map v.source_ip (fun f ->
             Aws.Query.Pair ("SourceIp", String.to_query f))
       ; Aws.Util.option_map v.destination_arn (fun f ->
             Aws.Query.Pair ("DestinationArn", String.to_query f))
       ; Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ; Aws.Util.option_map v.destination (fun f ->
             Aws.Query.Pair ("Destination", String.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Aws.Util.option_map v.created_date (fun f ->
             Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.network_insights_path_arn (fun f ->
             Aws.Query.Pair ("NetworkInsightsPathArn", String.to_query f))
       ; Aws.Util.option_map v.network_insights_path_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsPathId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.filter_at_destination (fun f ->
             "filterAtDestination", PathFilter.to_json f)
       ; Aws.Util.option_map v.filter_at_source (fun f ->
             "filterAtSource", PathFilter.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.destination_port (fun f ->
             "destinationPort", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", Protocol.to_json f)
       ; Aws.Util.option_map v.destination_ip (fun f -> "destinationIp", String.to_json f)
       ; Aws.Util.option_map v.source_ip (fun f -> "sourceIp", String.to_json f)
       ; Aws.Util.option_map v.destination_arn (fun f ->
             "destinationArn", String.to_json f)
       ; Aws.Util.option_map v.source_arn (fun f -> "sourceArn", String.to_json f)
       ; Aws.Util.option_map v.destination (fun f -> "destination", String.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "source", String.to_json f)
       ; Aws.Util.option_map v.created_date (fun f -> "createdDate", DateTime.to_json f)
       ; Aws.Util.option_map v.network_insights_path_arn (fun f ->
             "networkInsightsPathArn", String.to_json f)
       ; Aws.Util.option_map v.network_insights_path_id (fun f ->
             "networkInsightsPathId", String.to_json f)
       ])

let of_json j =
  { network_insights_path_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInsightsPathId") String.of_json
  ; network_insights_path_arn =
      Aws.Util.option_map (Aws.Json.lookup j "networkInsightsPathArn") String.of_json
  ; created_date = Aws.Util.option_map (Aws.Json.lookup j "createdDate") DateTime.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "source") String.of_json
  ; destination = Aws.Util.option_map (Aws.Json.lookup j "destination") String.of_json
  ; source_arn = Aws.Util.option_map (Aws.Json.lookup j "sourceArn") String.of_json
  ; destination_arn =
      Aws.Util.option_map (Aws.Json.lookup j "destinationArn") String.of_json
  ; source_ip = Aws.Util.option_map (Aws.Json.lookup j "sourceIp") String.of_json
  ; destination_ip =
      Aws.Util.option_map (Aws.Json.lookup j "destinationIp") String.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") Protocol.of_json
  ; destination_port =
      Aws.Util.option_map (Aws.Json.lookup j "destinationPort") Integer.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; filter_at_source =
      Aws.Util.option_map (Aws.Json.lookup j "filterAtSource") PathFilter.of_json
  ; filter_at_destination =
      Aws.Util.option_map (Aws.Json.lookup j "filterAtDestination") PathFilter.of_json
  }
