open Aws.BaseTypes

type t =
  { cidr_block : String.t option
  ; egress : Boolean.t option
  ; icmp_type_code : IcmpTypeCode.t option
  ; ipv6_cidr_block : String.t option
  ; port_range : PortRange.t option
  ; protocol : String.t option
  ; rule_action : RuleAction.t option
  ; rule_number : Integer.t option
  }

let make
    ?cidr_block
    ?egress
    ?icmp_type_code
    ?ipv6_cidr_block
    ?port_range
    ?protocol
    ?rule_action
    ?rule_number
    () =
  { cidr_block
  ; egress
  ; icmp_type_code
  ; ipv6_cidr_block
  ; port_range
  ; protocol
  ; rule_action
  ; rule_number
  }

let parse xml =
  Some
    { cidr_block = Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse
    ; egress = Aws.Util.option_bind (Aws.Xml.member "egress" xml) Boolean.parse
    ; icmp_type_code =
        Aws.Util.option_bind (Aws.Xml.member "icmpTypeCode" xml) IcmpTypeCode.parse
    ; ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "ipv6CidrBlock" xml) String.parse
    ; port_range = Aws.Util.option_bind (Aws.Xml.member "portRange" xml) PortRange.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    ; rule_action =
        Aws.Util.option_bind (Aws.Xml.member "ruleAction" xml) RuleAction.parse
    ; rule_number = Aws.Util.option_bind (Aws.Xml.member "ruleNumber" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_number (fun f ->
             Aws.Query.Pair ("RuleNumber", Integer.to_query f))
       ; Aws.Util.option_map v.rule_action (fun f ->
             Aws.Query.Pair ("RuleAction", RuleAction.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Aws.Util.option_map v.port_range (fun f ->
             Aws.Query.Pair ("PortRange", PortRange.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.icmp_type_code (fun f ->
             Aws.Query.Pair ("IcmpTypeCode", IcmpTypeCode.to_query f))
       ; Aws.Util.option_map v.egress (fun f ->
             Aws.Query.Pair ("Egress", Boolean.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_number (fun f -> "ruleNumber", Integer.to_json f)
       ; Aws.Util.option_map v.rule_action (fun f -> "ruleAction", RuleAction.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Aws.Util.option_map v.port_range (fun f -> "portRange", PortRange.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "ipv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.icmp_type_code (fun f ->
             "icmpTypeCode", IcmpTypeCode.to_json f)
       ; Aws.Util.option_map v.egress (fun f -> "egress", Boolean.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "cidrBlock", String.to_json f)
       ])

let of_json j =
  { cidr_block = Aws.Util.option_map (Aws.Json.lookup j "cidrBlock") String.of_json
  ; egress = Aws.Util.option_map (Aws.Json.lookup j "egress") Boolean.of_json
  ; icmp_type_code =
      Aws.Util.option_map (Aws.Json.lookup j "icmpTypeCode") IcmpTypeCode.of_json
  ; ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6CidrBlock") String.of_json
  ; port_range = Aws.Util.option_map (Aws.Json.lookup j "portRange") PortRange.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  ; rule_action = Aws.Util.option_map (Aws.Json.lookup j "ruleAction") RuleAction.of_json
  ; rule_number = Aws.Util.option_map (Aws.Json.lookup j "ruleNumber") Integer.of_json
  }
