open Aws.BaseTypes

type t =
  { replacement_strategy : FleetReplacementStrategy.t option
  ; termination_delay : Integer.t option
  }

let make ?replacement_strategy ?termination_delay () =
  { replacement_strategy; termination_delay }

let parse xml =
  Some
    { replacement_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "replacementStrategy" xml)
          FleetReplacementStrategy.parse
    ; termination_delay =
        Aws.Util.option_bind (Aws.Xml.member "terminationDelay" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.termination_delay (fun f ->
             Aws.Query.Pair ("TerminationDelay", Integer.to_query f))
       ; Aws.Util.option_map v.replacement_strategy (fun f ->
             Aws.Query.Pair ("ReplacementStrategy", FleetReplacementStrategy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.termination_delay (fun f ->
             "terminationDelay", Integer.to_json f)
       ; Aws.Util.option_map v.replacement_strategy (fun f ->
             "replacementStrategy", FleetReplacementStrategy.to_json f)
       ])

let of_json j =
  { replacement_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "replacementStrategy")
        FleetReplacementStrategy.of_json
  ; termination_delay =
      Aws.Util.option_map (Aws.Json.lookup j "terminationDelay") Integer.of_json
  }
