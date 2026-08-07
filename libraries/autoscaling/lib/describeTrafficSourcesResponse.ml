open Aws.BaseTypes

type t =
  { traffic_sources : TrafficSourceStates.t
  ; next_token : String.t option
  }

let make ?(traffic_sources = []) ?next_token () = { traffic_sources; next_token }

let parse xml =
  Some
    { traffic_sources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficSources" xml)
             TrafficSourceStates.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrafficSources.member", TrafficSourceStates.to_query v.traffic_sources))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("TrafficSources", TrafficSourceStates.to_json v.traffic_sources)
       ])

let of_json j =
  { traffic_sources =
      TrafficSourceStates.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficSources"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
