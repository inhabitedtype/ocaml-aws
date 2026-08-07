open Aws.BaseTypes

type t =
  { security_group_rule_id : String.t option
  ; group_id : String.t option
  ; group_owner_id : String.t option
  ; is_egress : Boolean.t option
  ; ip_protocol : String.t option
  ; from_port : Integer.t option
  ; to_port : Integer.t option
  ; cidr_ipv4 : String.t option
  ; cidr_ipv6 : String.t option
  ; prefix_list_id : String.t option
  ; referenced_group_info : ReferencedSecurityGroup.t option
  ; description : String.t option
  ; tags : TagList.t
  ; security_group_rule_arn : String.t option
  }

let make
    ?security_group_rule_id
    ?group_id
    ?group_owner_id
    ?is_egress
    ?ip_protocol
    ?from_port
    ?to_port
    ?cidr_ipv4
    ?cidr_ipv6
    ?prefix_list_id
    ?referenced_group_info
    ?description
    ?(tags = [])
    ?security_group_rule_arn
    () =
  { security_group_rule_id
  ; group_id
  ; group_owner_id
  ; is_egress
  ; ip_protocol
  ; from_port
  ; to_port
  ; cidr_ipv4
  ; cidr_ipv6
  ; prefix_list_id
  ; referenced_group_info
  ; description
  ; tags
  ; security_group_rule_arn
  }

let parse xml =
  Some
    { security_group_rule_id =
        Aws.Util.option_bind (Aws.Xml.member "securityGroupRuleId" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; group_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "groupOwnerId" xml) String.parse
    ; is_egress = Aws.Util.option_bind (Aws.Xml.member "isEgress" xml) Boolean.parse
    ; ip_protocol = Aws.Util.option_bind (Aws.Xml.member "ipProtocol" xml) String.parse
    ; from_port = Aws.Util.option_bind (Aws.Xml.member "fromPort" xml) Integer.parse
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "toPort" xml) Integer.parse
    ; cidr_ipv4 = Aws.Util.option_bind (Aws.Xml.member "cidrIpv4" xml) String.parse
    ; cidr_ipv6 = Aws.Util.option_bind (Aws.Xml.member "cidrIpv6" xml) String.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; referenced_group_info =
        Aws.Util.option_bind
          (Aws.Xml.member "referencedGroupInfo" xml)
          ReferencedSecurityGroup.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; security_group_rule_arn =
        Aws.Util.option_bind (Aws.Xml.member "securityGroupRuleArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.security_group_rule_arn (fun f ->
             Aws.Query.Pair ("SecurityGroupRuleArn", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.referenced_group_info (fun f ->
             Aws.Query.Pair ("ReferencedGroupInfo", ReferencedSecurityGroup.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.cidr_ipv6 (fun f ->
             Aws.Query.Pair ("CidrIpv6", String.to_query f))
       ; Aws.Util.option_map v.cidr_ipv4 (fun f ->
             Aws.Query.Pair ("CidrIpv4", String.to_query f))
       ; Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ; Aws.Util.option_map v.ip_protocol (fun f ->
             Aws.Query.Pair ("IpProtocol", String.to_query f))
       ; Aws.Util.option_map v.is_egress (fun f ->
             Aws.Query.Pair ("IsEgress", Boolean.to_query f))
       ; Aws.Util.option_map v.group_owner_id (fun f ->
             Aws.Query.Pair ("GroupOwnerId", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.security_group_rule_id (fun f ->
             Aws.Query.Pair ("SecurityGroupRuleId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.security_group_rule_arn (fun f ->
             "securityGroupRuleArn", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.referenced_group_info (fun f ->
             "referencedGroupInfo", ReferencedSecurityGroup.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Aws.Util.option_map v.cidr_ipv6 (fun f -> "cidrIpv6", String.to_json f)
       ; Aws.Util.option_map v.cidr_ipv4 (fun f -> "cidrIpv4", String.to_json f)
       ; Aws.Util.option_map v.to_port (fun f -> "toPort", Integer.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "fromPort", Integer.to_json f)
       ; Aws.Util.option_map v.ip_protocol (fun f -> "ipProtocol", String.to_json f)
       ; Aws.Util.option_map v.is_egress (fun f -> "isEgress", Boolean.to_json f)
       ; Aws.Util.option_map v.group_owner_id (fun f -> "groupOwnerId", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.security_group_rule_id (fun f ->
             "securityGroupRuleId", String.to_json f)
       ])

let of_json j =
  { security_group_rule_id =
      Aws.Util.option_map (Aws.Json.lookup j "securityGroupRuleId") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; group_owner_id = Aws.Util.option_map (Aws.Json.lookup j "groupOwnerId") String.of_json
  ; is_egress = Aws.Util.option_map (Aws.Json.lookup j "isEgress") Boolean.of_json
  ; ip_protocol = Aws.Util.option_map (Aws.Json.lookup j "ipProtocol") String.of_json
  ; from_port = Aws.Util.option_map (Aws.Json.lookup j "fromPort") Integer.of_json
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "toPort") Integer.of_json
  ; cidr_ipv4 = Aws.Util.option_map (Aws.Json.lookup j "cidrIpv4") String.of_json
  ; cidr_ipv6 = Aws.Util.option_map (Aws.Json.lookup j "cidrIpv6") String.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; referenced_group_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "referencedGroupInfo")
        ReferencedSecurityGroup.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; security_group_rule_arn =
      Aws.Util.option_map (Aws.Json.lookup j "securityGroupRuleArn") String.of_json
  }
