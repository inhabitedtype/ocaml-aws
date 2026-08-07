open Aws.BaseTypes

type t =
  { source_address : String.t option
  ; source_port_range : FilterPortRange.t option
  ; destination_address : String.t option
  ; destination_port_range : FilterPortRange.t option
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
        Aws.Util.option_bind (Aws.Xml.member "sourceAddress" xml) String.parse
    ; source_port_range =
        Aws.Util.option_bind (Aws.Xml.member "sourcePortRange" xml) FilterPortRange.parse
    ; destination_address =
        Aws.Util.option_bind (Aws.Xml.member "destinationAddress" xml) String.parse
    ; destination_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "destinationPortRange" xml)
          FilterPortRange.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair ("DestinationPortRange", FilterPortRange.to_query f))
       ; Aws.Util.option_map v.destination_address (fun f ->
             Aws.Query.Pair ("DestinationAddress", String.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", FilterPortRange.to_query f))
       ; Aws.Util.option_map v.source_address (fun f ->
             Aws.Query.Pair ("SourceAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_port_range (fun f ->
             "destinationPortRange", FilterPortRange.to_json f)
       ; Aws.Util.option_map v.destination_address (fun f ->
             "destinationAddress", String.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "sourcePortRange", FilterPortRange.to_json f)
       ; Aws.Util.option_map v.source_address (fun f -> "sourceAddress", String.to_json f)
       ])

let of_json j =
  { source_address =
      Aws.Util.option_map (Aws.Json.lookup j "sourceAddress") String.of_json
  ; source_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "sourcePortRange") FilterPortRange.of_json
  ; destination_address =
      Aws.Util.option_map (Aws.Json.lookup j "destinationAddress") String.of_json
  ; destination_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "destinationPortRange")
        FilterPortRange.of_json
  }
