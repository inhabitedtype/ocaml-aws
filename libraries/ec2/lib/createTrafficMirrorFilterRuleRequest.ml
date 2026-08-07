open Aws.BaseTypes

type t =
  { traffic_mirror_filter_id : String.t
  ; traffic_direction : TrafficDirection.t
  ; rule_number : Integer.t
  ; rule_action : TrafficMirrorRuleAction.t
  ; destination_port_range : TrafficMirrorPortRangeRequest.t option
  ; source_port_range : TrafficMirrorPortRangeRequest.t option
  ; protocol : Integer.t option
  ; destination_cidr_block : String.t
  ; source_cidr_block : String.t
  ; description : String.t option
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~traffic_mirror_filter_id
    ~traffic_direction
    ~rule_number
    ~rule_action
    ?destination_port_range
    ?source_port_range
    ?protocol
    ~destination_cidr_block
    ~source_cidr_block
    ?description
    ?dry_run
    ?client_token
    ?(tag_specifications = [])
    () =
  { traffic_mirror_filter_id
  ; traffic_direction
  ; rule_number
  ; rule_action
  ; destination_port_range
  ; source_port_range
  ; protocol
  ; destination_cidr_block
  ; source_cidr_block
  ; description
  ; dry_run
  ; client_token
  ; tag_specifications
  }

let parse xml =
  Some
    { traffic_mirror_filter_id =
        Aws.Xml.required
          "TrafficMirrorFilterId"
          (Aws.Util.option_bind (Aws.Xml.member "TrafficMirrorFilterId" xml) String.parse)
    ; traffic_direction =
        Aws.Xml.required
          "TrafficDirection"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficDirection" xml)
             TrafficDirection.parse)
    ; rule_number =
        Aws.Xml.required
          "RuleNumber"
          (Aws.Util.option_bind (Aws.Xml.member "RuleNumber" xml) Integer.parse)
    ; rule_action =
        Aws.Xml.required
          "RuleAction"
          (Aws.Util.option_bind
             (Aws.Xml.member "RuleAction" xml)
             TrafficMirrorRuleAction.parse)
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
        Aws.Xml.required
          "DestinationCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse)
    ; source_cidr_block =
        Aws.Xml.required
          "SourceCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "SourceCidrBlock" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("SourceCidrBlock", String.to_query v.source_cidr_block))
       ; Some
           (Aws.Query.Pair
              ("DestinationCidrBlock", String.to_query v.destination_cidr_block))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", Integer.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", TrafficMirrorPortRangeRequest.to_query f))
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair
               ("DestinationPortRange", TrafficMirrorPortRangeRequest.to_query f))
       ; Some
           (Aws.Query.Pair ("RuleAction", TrafficMirrorRuleAction.to_query v.rule_action))
       ; Some (Aws.Query.Pair ("RuleNumber", Integer.to_query v.rule_number))
       ; Some
           (Aws.Query.Pair
              ("TrafficDirection", TrafficDirection.to_query v.traffic_direction))
       ; Some
           (Aws.Query.Pair
              ("TrafficMirrorFilterId", String.to_query v.traffic_mirror_filter_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("SourceCidrBlock", String.to_json v.source_cidr_block)
       ; Some ("DestinationCidrBlock", String.to_json v.destination_cidr_block)
       ; Aws.Util.option_map v.protocol (fun f -> "Protocol", Integer.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "SourcePortRange", TrafficMirrorPortRangeRequest.to_json f)
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             "DestinationPortRange", TrafficMirrorPortRangeRequest.to_json f)
       ; Some ("RuleAction", TrafficMirrorRuleAction.to_json v.rule_action)
       ; Some ("RuleNumber", Integer.to_json v.rule_number)
       ; Some ("TrafficDirection", TrafficDirection.to_json v.traffic_direction)
       ; Some ("TrafficMirrorFilterId", String.to_json v.traffic_mirror_filter_id)
       ])

let of_json j =
  { traffic_mirror_filter_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorFilterId"))
  ; traffic_direction =
      TrafficDirection.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficDirection"))
  ; rule_number =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleNumber"))
  ; rule_action =
      TrafficMirrorRuleAction.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleAction"))
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
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationCidrBlock"))
  ; source_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceCidrBlock"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
