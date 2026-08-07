open Aws.BaseTypes

type t =
  { rule_group_arn : String.t option
  ; sources : ValueStringList.t
  ; destinations : ValueStringList.t
  ; source_ports : PortRangeList.t
  ; destination_ports : PortRangeList.t
  ; protocol : String.t option
  ; rule_action : String.t option
  ; direction : String.t option
  }

let make
    ?rule_group_arn
    ?(sources = [])
    ?(destinations = [])
    ?(source_ports = [])
    ?(destination_ports = [])
    ?protocol
    ?rule_action
    ?direction
    () =
  { rule_group_arn
  ; sources
  ; destinations
  ; source_ports
  ; destination_ports
  ; protocol
  ; rule_action
  ; direction
  }

let parse xml =
  Some
    { rule_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "ruleGroupArn" xml) String.parse
    ; sources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "sourceSet" xml) ValueStringList.parse)
    ; destinations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationSet" xml)
             ValueStringList.parse)
    ; source_ports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "sourcePortSet" xml) PortRangeList.parse)
    ; destination_ports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "destinationPortSet" xml)
             PortRangeList.parse)
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    ; rule_action = Aws.Util.option_bind (Aws.Xml.member "ruleAction" xml) String.parse
    ; direction = Aws.Util.option_bind (Aws.Xml.member "direction" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.direction (fun f ->
             Aws.Query.Pair ("Direction", String.to_query f))
       ; Aws.Util.option_map v.rule_action (fun f ->
             Aws.Query.Pair ("RuleAction", String.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DestinationPortSet", PortRangeList.to_query v.destination_ports))
       ; Some (Aws.Query.Pair ("SourcePortSet", PortRangeList.to_query v.source_ports))
       ; Some (Aws.Query.Pair ("DestinationSet", ValueStringList.to_query v.destinations))
       ; Some (Aws.Query.Pair ("SourceSet", ValueStringList.to_query v.sources))
       ; Aws.Util.option_map v.rule_group_arn (fun f ->
             Aws.Query.Pair ("RuleGroupArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.direction (fun f -> "direction", String.to_json f)
       ; Aws.Util.option_map v.rule_action (fun f -> "ruleAction", String.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Some ("destinationPortSet", PortRangeList.to_json v.destination_ports)
       ; Some ("sourcePortSet", PortRangeList.to_json v.source_ports)
       ; Some ("destinationSet", ValueStringList.to_json v.destinations)
       ; Some ("sourceSet", ValueStringList.to_json v.sources)
       ; Aws.Util.option_map v.rule_group_arn (fun f -> "ruleGroupArn", String.to_json f)
       ])

let of_json j =
  { rule_group_arn = Aws.Util.option_map (Aws.Json.lookup j "ruleGroupArn") String.of_json
  ; sources =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "sourceSet"))
  ; destinations =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationSet"))
  ; source_ports =
      PortRangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "sourcePortSet"))
  ; destination_ports =
      PortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "destinationPortSet"))
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  ; rule_action = Aws.Util.option_map (Aws.Json.lookup j "ruleAction") String.of_json
  ; direction = Aws.Util.option_map (Aws.Json.lookup j "direction") String.of_json
  }
