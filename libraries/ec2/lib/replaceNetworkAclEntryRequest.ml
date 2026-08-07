open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; network_acl_id : String.t
  ; rule_number : Integer.t
  ; protocol : String.t
  ; rule_action : RuleAction.t
  ; egress : Boolean.t
  ; cidr_block : String.t option
  ; ipv6_cidr_block : String.t option
  ; icmp_type_code : IcmpTypeCode.t option
  ; port_range : PortRange.t option
  }

let make
    ?dry_run
    ~network_acl_id
    ~rule_number
    ~protocol
    ~rule_action
    ~egress
    ?cidr_block
    ?ipv6_cidr_block
    ?icmp_type_code
    ?port_range
    () =
  { dry_run
  ; network_acl_id
  ; rule_number
  ; protocol
  ; rule_action
  ; egress
  ; cidr_block
  ; ipv6_cidr_block
  ; icmp_type_code
  ; port_range
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_acl_id =
        Aws.Xml.required
          "networkAclId"
          (Aws.Util.option_bind (Aws.Xml.member "networkAclId" xml) String.parse)
    ; rule_number =
        Aws.Xml.required
          "ruleNumber"
          (Aws.Util.option_bind (Aws.Xml.member "ruleNumber" xml) Integer.parse)
    ; protocol =
        Aws.Xml.required
          "protocol"
          (Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse)
    ; rule_action =
        Aws.Xml.required
          "ruleAction"
          (Aws.Util.option_bind (Aws.Xml.member "ruleAction" xml) RuleAction.parse)
    ; egress =
        Aws.Xml.required
          "egress"
          (Aws.Util.option_bind (Aws.Xml.member "egress" xml) Boolean.parse)
    ; cidr_block = Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse
    ; ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "ipv6CidrBlock" xml) String.parse
    ; icmp_type_code = Aws.Util.option_bind (Aws.Xml.member "Icmp" xml) IcmpTypeCode.parse
    ; port_range = Aws.Util.option_bind (Aws.Xml.member "portRange" xml) PortRange.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.port_range (fun f ->
             Aws.Query.Pair ("PortRange", PortRange.to_query f))
       ; Aws.Util.option_map v.icmp_type_code (fun f ->
             Aws.Query.Pair ("Icmp", IcmpTypeCode.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ; Some (Aws.Query.Pair ("Egress", Boolean.to_query v.egress))
       ; Some (Aws.Query.Pair ("RuleAction", RuleAction.to_query v.rule_action))
       ; Some (Aws.Query.Pair ("Protocol", String.to_query v.protocol))
       ; Some (Aws.Query.Pair ("RuleNumber", Integer.to_query v.rule_number))
       ; Some (Aws.Query.Pair ("NetworkAclId", String.to_query v.network_acl_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.port_range (fun f -> "portRange", PortRange.to_json f)
       ; Aws.Util.option_map v.icmp_type_code (fun f -> "Icmp", IcmpTypeCode.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "ipv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "cidrBlock", String.to_json f)
       ; Some ("egress", Boolean.to_json v.egress)
       ; Some ("ruleAction", RuleAction.to_json v.rule_action)
       ; Some ("protocol", String.to_json v.protocol)
       ; Some ("ruleNumber", Integer.to_json v.rule_number)
       ; Some ("networkAclId", String.to_json v.network_acl_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_acl_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkAclId"))
  ; rule_number =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleNumber"))
  ; protocol = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "protocol"))
  ; rule_action =
      RuleAction.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleAction"))
  ; egress = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "egress"))
  ; cidr_block = Aws.Util.option_map (Aws.Json.lookup j "cidrBlock") String.of_json
  ; ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6CidrBlock") String.of_json
  ; icmp_type_code = Aws.Util.option_map (Aws.Json.lookup j "Icmp") IcmpTypeCode.of_json
  ; port_range = Aws.Util.option_map (Aws.Json.lookup j "portRange") PortRange.of_json
  }
