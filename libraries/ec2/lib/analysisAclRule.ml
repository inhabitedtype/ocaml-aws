open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; egress : Boolean.t option
  ; port_range : PortRange.t option
  ; protocol : String.t option
  ; rule_action : String.t option
  ; rule_number : Integer.t option
  }

let make ?cidr ?egress ?port_range ?protocol ?rule_action ?rule_number () =
  { cidr; egress; port_range; protocol; rule_action; rule_number }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; egress = Aws.Util.option_bind (Aws.Xml.member "egress" xml) Boolean.parse
    ; port_range = Aws.Util.option_bind (Aws.Xml.member "portRange" xml) PortRange.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    ; rule_action = Aws.Util.option_bind (Aws.Xml.member "ruleAction" xml) String.parse
    ; rule_number = Aws.Util.option_bind (Aws.Xml.member "ruleNumber" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_number (fun f ->
             Aws.Query.Pair ("RuleNumber", Integer.to_query f))
       ; Aws.Util.option_map v.rule_action (fun f ->
             Aws.Query.Pair ("RuleAction", String.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Aws.Util.option_map v.port_range (fun f ->
             Aws.Query.Pair ("PortRange", PortRange.to_query f))
       ; Aws.Util.option_map v.egress (fun f ->
             Aws.Query.Pair ("Egress", Boolean.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_number (fun f -> "ruleNumber", Integer.to_json f)
       ; Aws.Util.option_map v.rule_action (fun f -> "ruleAction", String.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Aws.Util.option_map v.port_range (fun f -> "portRange", PortRange.to_json f)
       ; Aws.Util.option_map v.egress (fun f -> "egress", Boolean.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; egress = Aws.Util.option_map (Aws.Json.lookup j "egress") Boolean.of_json
  ; port_range = Aws.Util.option_map (Aws.Json.lookup j "portRange") PortRange.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  ; rule_action = Aws.Util.option_map (Aws.Json.lookup j "ruleAction") String.of_json
  ; rule_number = Aws.Util.option_map (Aws.Json.lookup j "ruleNumber") Integer.of_json
  }
