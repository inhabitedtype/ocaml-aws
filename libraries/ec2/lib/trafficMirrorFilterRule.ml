open Aws.BaseTypes

type t =
  { traffic_mirror_filter_rule_id : String.t option
  ; traffic_mirror_filter_id : String.t option
  ; traffic_direction : TrafficDirection.t option
  ; rule_number : Integer.t option
  ; rule_action : TrafficMirrorRuleAction.t option
  ; protocol : Integer.t option
  ; destination_port_range : TrafficMirrorPortRange.t option
  ; source_port_range : TrafficMirrorPortRange.t option
  ; destination_cidr_block : String.t option
  ; source_cidr_block : String.t option
  ; description : String.t option
  ; tags : TagList.t
  }

let make
    ?traffic_mirror_filter_rule_id
    ?traffic_mirror_filter_id
    ?traffic_direction
    ?rule_number
    ?rule_action
    ?protocol
    ?destination_port_range
    ?source_port_range
    ?destination_cidr_block
    ?source_cidr_block
    ?description
    ?(tags = [])
    () =
  { traffic_mirror_filter_rule_id
  ; traffic_mirror_filter_id
  ; traffic_direction
  ; rule_number
  ; rule_action
  ; protocol
  ; destination_port_range
  ; source_port_range
  ; destination_cidr_block
  ; source_cidr_block
  ; description
  ; tags
  }

let parse xml =
  Some
    { traffic_mirror_filter_rule_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorFilterRuleId" xml) String.parse
    ; traffic_mirror_filter_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorFilterId" xml) String.parse
    ; traffic_direction =
        Aws.Util.option_bind
          (Aws.Xml.member "trafficDirection" xml)
          TrafficDirection.parse
    ; rule_number = Aws.Util.option_bind (Aws.Xml.member "ruleNumber" xml) Integer.parse
    ; rule_action =
        Aws.Util.option_bind
          (Aws.Xml.member "ruleAction" xml)
          TrafficMirrorRuleAction.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) Integer.parse
    ; destination_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "destinationPortRange" xml)
          TrafficMirrorPortRange.parse
    ; source_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "sourcePortRange" xml)
          TrafficMirrorPortRange.parse
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; source_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "sourceCidrBlock" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             Aws.Query.Pair ("SourceCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", TrafficMirrorPortRange.to_query f))
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair ("DestinationPortRange", TrafficMirrorPortRange.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", Integer.to_query f))
       ; Aws.Util.option_map v.rule_action (fun f ->
             Aws.Query.Pair ("RuleAction", TrafficMirrorRuleAction.to_query f))
       ; Aws.Util.option_map v.rule_number (fun f ->
             Aws.Query.Pair ("RuleNumber", Integer.to_query f))
       ; Aws.Util.option_map v.traffic_direction (fun f ->
             Aws.Query.Pair ("TrafficDirection", TrafficDirection.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterId", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_filter_rule_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorFilterRuleId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             "sourceCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "sourcePortRange", TrafficMirrorPortRange.to_json f)
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             "destinationPortRange", TrafficMirrorPortRange.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", Integer.to_json f)
       ; Aws.Util.option_map v.rule_action (fun f ->
             "ruleAction", TrafficMirrorRuleAction.to_json f)
       ; Aws.Util.option_map v.rule_number (fun f -> "ruleNumber", Integer.to_json f)
       ; Aws.Util.option_map v.traffic_direction (fun f ->
             "trafficDirection", TrafficDirection.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_filter_id (fun f ->
             "trafficMirrorFilterId", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_filter_rule_id (fun f ->
             "trafficMirrorFilterRuleId", String.to_json f)
       ])

let of_json j =
  { traffic_mirror_filter_rule_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorFilterRuleId") String.of_json
  ; traffic_mirror_filter_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorFilterId") String.of_json
  ; traffic_direction =
      Aws.Util.option_map (Aws.Json.lookup j "trafficDirection") TrafficDirection.of_json
  ; rule_number = Aws.Util.option_map (Aws.Json.lookup j "ruleNumber") Integer.of_json
  ; rule_action =
      Aws.Util.option_map (Aws.Json.lookup j "ruleAction") TrafficMirrorRuleAction.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") Integer.of_json
  ; destination_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "destinationPortRange")
        TrafficMirrorPortRange.of_json
  ; source_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourcePortRange")
        TrafficMirrorPortRange.of_json
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; source_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "sourceCidrBlock") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
