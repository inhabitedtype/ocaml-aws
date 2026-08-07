open Aws.BaseTypes

type t =
  { source_address : String.t option
  ; source_port_range : RequestFilterPortRange.t option
  ; destination_address : String.t option
  ; destination_port_range : RequestFilterPortRange.t option
  }

let make
    ?source_address
    ?source_port_range
    ?destination_address
    ?destination_port_range
    () =
  { source_address; source_port_range; destination_address; destination_port_range }

let parse xml =
  Some
    { source_address =
        Aws.Util.option_bind (Aws.Xml.member "SourceAddress" xml) String.parse
    ; source_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "SourcePortRange" xml)
          RequestFilterPortRange.parse
    ; destination_address =
        Aws.Util.option_bind (Aws.Xml.member "DestinationAddress" xml) String.parse
    ; destination_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationPortRange" xml)
          RequestFilterPortRange.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair ("DestinationPortRange", RequestFilterPortRange.to_query f))
       ; Aws.Util.option_map v.destination_address (fun f ->
             Aws.Query.Pair ("DestinationAddress", String.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", RequestFilterPortRange.to_query f))
       ; Aws.Util.option_map v.source_address (fun f ->
             Aws.Query.Pair ("SourceAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_port_range (fun f ->
             "DestinationPortRange", RequestFilterPortRange.to_json f)
       ; Aws.Util.option_map v.destination_address (fun f ->
             "DestinationAddress", String.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "SourcePortRange", RequestFilterPortRange.to_json f)
       ; Aws.Util.option_map v.source_address (fun f -> "SourceAddress", String.to_json f)
       ])

let of_json j =
  { source_address =
      Aws.Util.option_map (Aws.Json.lookup j "SourceAddress") String.of_json
  ; source_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourcePortRange")
        RequestFilterPortRange.of_json
  ; destination_address =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationAddress") String.of_json
  ; destination_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationPortRange")
        RequestFilterPortRange.of_json
  }
