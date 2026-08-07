open Aws.BaseTypes

type t =
  { traffic_mirror_filter_rule_id : String.t
  ; traffic_direction : TrafficDirection.t option
  ; rule_number : Integer.t option
  ; rule_action : TrafficMirrorRuleAction.t option
  ; destination_port_range : TrafficMirrorPortRangeRequest.t option
  ; source_port_range : TrafficMirrorPortRangeRequest.t option
  ; protocol : Integer.t option
  ; destination_cidr_block : String.t option
  ; source_cidr_block : String.t option
  ; description : String.t option
  ; remove_fields : TrafficMirrorFilterRuleFieldList.t
  ; dry_run : Boolean.t option
  }

let make
    ~traffic_mirror_filter_rule_id
    ?traffic_direction
    ?rule_number
    ?rule_action
    ?destination_port_range
    ?source_port_range
    ?protocol
    ?destination_cidr_block
    ?source_cidr_block
    ?description
    ?(remove_fields = [])
    ?dry_run
    () =
  { traffic_mirror_filter_rule_id
  ; traffic_direction
  ; rule_number
  ; rule_action
  ; destination_port_range
  ; source_port_range
  ; protocol
  ; destination_cidr_block
  ; source_cidr_block
  ; description
  ; remove_fields
  ; dry_run
  }

let parse xml =
  Some
    { traffic_mirror_filter_rule_id =
        Aws.Xml.required
          "TrafficMirrorFilterRuleId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficMirrorFilterRuleId" xml)
             String.parse)
    ; traffic_direction =
        Aws.Util.option_bind
          (Aws.Xml.member "TrafficDirection" xml)
          TrafficDirection.parse
    ; rule_number = Aws.Util.option_bind (Aws.Xml.member "RuleNumber" xml) Integer.parse
    ; rule_action =
        Aws.Util.option_bind
          (Aws.Xml.member "RuleAction" xml)
          TrafficMirrorRuleAction.parse
    ; destination_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationPortRange" xml)
          TrafficMirrorPortRangeRequest.parse
    ; source_port_range =
        Aws.Util.option_bind
          (Aws.Xml.member "SourcePortRange" xml)
          TrafficMirrorPortRangeRequest.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) Integer.parse
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse
    ; source_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "SourceCidrBlock" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; remove_fields =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveField" xml)
             TrafficMirrorFilterRuleFieldList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RemoveField", TrafficMirrorFilterRuleFieldList.to_query v.remove_fields))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             Aws.Query.Pair ("SourceCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", Integer.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", TrafficMirrorPortRangeRequest.to_query f))
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair
               ("DestinationPortRange", TrafficMirrorPortRangeRequest.to_query f))
       ; Aws.Util.option_map v.rule_action (fun f ->
             Aws.Query.Pair ("RuleAction", TrafficMirrorRuleAction.to_query f))
       ; Aws.Util.option_map v.rule_number (fun f ->
             Aws.Query.Pair ("RuleNumber", Integer.to_query f))
       ; Aws.Util.option_map v.traffic_direction (fun f ->
             Aws.Query.Pair ("TrafficDirection", TrafficDirection.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TrafficMirrorFilterRuleId"
              , String.to_query v.traffic_mirror_filter_rule_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("RemoveField", TrafficMirrorFilterRuleFieldList.to_json v.remove_fields)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             "SourceCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "DestinationCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "Protocol", Integer.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "SourcePortRange", TrafficMirrorPortRangeRequest.to_json f)
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             "DestinationPortRange", TrafficMirrorPortRangeRequest.to_json f)
       ; Aws.Util.option_map v.rule_action (fun f ->
             "RuleAction", TrafficMirrorRuleAction.to_json f)
       ; Aws.Util.option_map v.rule_number (fun f -> "RuleNumber", Integer.to_json f)
       ; Aws.Util.option_map v.traffic_direction (fun f ->
             "TrafficDirection", TrafficDirection.to_json f)
       ; Some ("TrafficMirrorFilterRuleId", String.to_json v.traffic_mirror_filter_rule_id)
       ])

let of_json j =
  { traffic_mirror_filter_rule_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorFilterRuleId"))
  ; traffic_direction =
      Aws.Util.option_map (Aws.Json.lookup j "TrafficDirection") TrafficDirection.of_json
  ; rule_number = Aws.Util.option_map (Aws.Json.lookup j "RuleNumber") Integer.of_json
  ; rule_action =
      Aws.Util.option_map (Aws.Json.lookup j "RuleAction") TrafficMirrorRuleAction.of_json
  ; destination_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationPortRange")
        TrafficMirrorPortRangeRequest.of_json
  ; source_port_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourcePortRange")
        TrafficMirrorPortRangeRequest.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "Protocol") Integer.of_json
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationCidrBlock") String.of_json
  ; source_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "SourceCidrBlock") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; remove_fields =
      TrafficMirrorFilterRuleFieldList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveField"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
