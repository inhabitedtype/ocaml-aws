open Aws.BaseTypes

type t =
  { source_ip : String.t option
  ; destination_ip : String.t option
  ; source : String.t
  ; destination : String.t option
  ; protocol : Protocol.t
  ; destination_port : Integer.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; client_token : String.t
  ; filter_at_source : PathRequestFilter.t option
  ; filter_at_destination : PathRequestFilter.t option
  }

let make
    ?source_ip
    ?destination_ip
    ~source
    ?destination
    ~protocol
    ?destination_port
    ?(tag_specifications = [])
    ?dry_run
    ~client_token
    ?filter_at_source
    ?filter_at_destination
    () =
  { source_ip
  ; destination_ip
  ; source
  ; destination
  ; protocol
  ; destination_port
  ; tag_specifications
  ; dry_run
  ; client_token
  ; filter_at_source
  ; filter_at_destination
  }

let parse xml =
  Some
    { source_ip = Aws.Util.option_bind (Aws.Xml.member "SourceIp" xml) String.parse
    ; destination_ip =
        Aws.Util.option_bind (Aws.Xml.member "DestinationIp" xml) String.parse
    ; source =
        Aws.Xml.required
          "Source"
          (Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse)
    ; destination = Aws.Util.option_bind (Aws.Xml.member "Destination" xml) String.parse
    ; protocol =
        Aws.Xml.required
          "Protocol"
          (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) Protocol.parse)
    ; destination_port =
        Aws.Util.option_bind (Aws.Xml.member "DestinationPort" xml) Integer.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token =
        Aws.Xml.required
          "ClientToken"
          (Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse)
    ; filter_at_source =
        Aws.Util.option_bind (Aws.Xml.member "FilterAtSource" xml) PathRequestFilter.parse
    ; filter_at_destination =
        Aws.Util.option_bind
          (Aws.Xml.member "FilterAtDestination" xml)
          PathRequestFilter.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.filter_at_destination (fun f ->
             Aws.Query.Pair ("FilterAtDestination", PathRequestFilter.to_query f))
       ; Aws.Util.option_map v.filter_at_source (fun f ->
             Aws.Query.Pair ("FilterAtSource", PathRequestFilter.to_query f))
       ; Some (Aws.Query.Pair ("ClientToken", String.to_query v.client_token))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.destination_port (fun f ->
             Aws.Query.Pair ("DestinationPort", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Protocol", Protocol.to_query v.protocol))
       ; Aws.Util.option_map v.destination (fun f ->
             Aws.Query.Pair ("Destination", String.to_query f))
       ; Some (Aws.Query.Pair ("Source", String.to_query v.source))
       ; Aws.Util.option_map v.destination_ip (fun f ->
             Aws.Query.Pair ("DestinationIp", String.to_query f))
       ; Aws.Util.option_map v.source_ip (fun f ->
             Aws.Query.Pair ("SourceIp", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.filter_at_destination (fun f ->
             "FilterAtDestination", PathRequestFilter.to_json f)
       ; Aws.Util.option_map v.filter_at_source (fun f ->
             "FilterAtSource", PathRequestFilter.to_json f)
       ; Some ("ClientToken", String.to_json v.client_token)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.destination_port (fun f ->
             "DestinationPort", Integer.to_json f)
       ; Some ("Protocol", Protocol.to_json v.protocol)
       ; Aws.Util.option_map v.destination (fun f -> "Destination", String.to_json f)
       ; Some ("Source", String.to_json v.source)
       ; Aws.Util.option_map v.destination_ip (fun f -> "DestinationIp", String.to_json f)
       ; Aws.Util.option_map v.source_ip (fun f -> "SourceIp", String.to_json f)
       ])

let of_json j =
  { source_ip = Aws.Util.option_map (Aws.Json.lookup j "SourceIp") String.of_json
  ; destination_ip =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationIp") String.of_json
  ; source = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Source"))
  ; destination = Aws.Util.option_map (Aws.Json.lookup j "Destination") String.of_json
  ; protocol = Protocol.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Protocol"))
  ; destination_port =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationPort") Integer.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientToken"))
  ; filter_at_source =
      Aws.Util.option_map (Aws.Json.lookup j "FilterAtSource") PathRequestFilter.of_json
  ; filter_at_destination =
      Aws.Util.option_map
        (Aws.Json.lookup j "FilterAtDestination")
        PathRequestFilter.of_json
  }
