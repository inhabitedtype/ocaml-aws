open Aws.BaseTypes

type t =
  { traffic_source : String.t option
  ; state : String.t option
  ; identifier : String.t option
  ; type_ : String.t option
  }

let make ?traffic_source ?state ?identifier ?type_ () =
  { traffic_source; state; identifier; type_ }

let parse xml =
  Some
    { traffic_source =
        Aws.Util.option_bind (Aws.Xml.member "TrafficSource" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
    ; identifier = Aws.Util.option_bind (Aws.Xml.member "Identifier" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Aws.Util.option_map v.identifier (fun f ->
             Aws.Query.Pair ("Identifier", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.traffic_source (fun f ->
             Aws.Query.Pair ("TrafficSource", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> "Type", String.to_json f)
       ; Aws.Util.option_map v.identifier (fun f -> "Identifier", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
       ; Aws.Util.option_map v.traffic_source (fun f -> "TrafficSource", String.to_json f)
       ])

let of_json j =
  { traffic_source =
      Aws.Util.option_map (Aws.Json.lookup j "TrafficSource") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
  ; identifier = Aws.Util.option_map (Aws.Json.lookup j "Identifier") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") String.of_json
  }
