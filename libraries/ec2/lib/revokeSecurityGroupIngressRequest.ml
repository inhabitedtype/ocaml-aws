open Aws.BaseTypes

type t =
  { cidr_ip : String.t option
  ; from_port : Integer.t option
  ; group_id : String.t option
  ; group_name : String.t option
  ; ip_permissions : IpPermissionList.t
  ; ip_protocol : String.t option
  ; source_security_group_name : String.t option
  ; source_security_group_owner_id : String.t option
  ; to_port : Integer.t option
  ; security_group_rule_ids : SecurityGroupRuleIdList.t
  ; dry_run : Boolean.t option
  }

let make
    ?cidr_ip
    ?from_port
    ?group_id
    ?group_name
    ?(ip_permissions = [])
    ?ip_protocol
    ?source_security_group_name
    ?source_security_group_owner_id
    ?to_port
    ?(security_group_rule_ids = [])
    ?dry_run
    () =
  { cidr_ip
  ; from_port
  ; group_id
  ; group_name
  ; ip_permissions
  ; ip_protocol
  ; source_security_group_name
  ; source_security_group_owner_id
  ; to_port
  ; security_group_rule_ids
  ; dry_run
  }

let parse xml =
  Some
    { cidr_ip = Aws.Util.option_bind (Aws.Xml.member "CidrIp" xml) String.parse
    ; from_port = Aws.Util.option_bind (Aws.Xml.member "FromPort" xml) Integer.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
    ; ip_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "IpPermissions" xml)
             IpPermissionList.parse)
    ; ip_protocol = Aws.Util.option_bind (Aws.Xml.member "IpProtocol" xml) String.parse
    ; source_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "SourceSecurityGroupName" xml) String.parse
    ; source_security_group_owner_id =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceSecurityGroupOwnerId" xml)
          String.parse
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "ToPort" xml) Integer.parse
    ; security_group_rule_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupRuleId" xml)
             SecurityGroupRuleIdList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupRuleId"
              , SecurityGroupRuleIdList.to_query v.security_group_rule_ids ))
       ; Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Aws.Util.option_map v.source_security_group_owner_id (fun f ->
             Aws.Query.Pair ("SourceSecurityGroupOwnerId", String.to_query f))
       ; Aws.Util.option_map v.source_security_group_name (fun f ->
             Aws.Query.Pair ("SourceSecurityGroupName", String.to_query f))
       ; Aws.Util.option_map v.ip_protocol (fun f ->
             Aws.Query.Pair ("IpProtocol", String.to_query f))
       ; Some
           (Aws.Query.Pair ("IpPermissions", IpPermissionList.to_query v.ip_permissions))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ; Aws.Util.option_map v.cidr_ip (fun f ->
             Aws.Query.Pair ("CidrIp", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some
           ( "SecurityGroupRuleId"
           , SecurityGroupRuleIdList.to_json v.security_group_rule_ids )
       ; Aws.Util.option_map v.to_port (fun f -> "ToPort", Integer.to_json f)
       ; Aws.Util.option_map v.source_security_group_owner_id (fun f ->
             "SourceSecurityGroupOwnerId", String.to_json f)
       ; Aws.Util.option_map v.source_security_group_name (fun f ->
             "SourceSecurityGroupName", String.to_json f)
       ; Aws.Util.option_map v.ip_protocol (fun f -> "IpProtocol", String.to_json f)
       ; Some ("IpPermissions", IpPermissionList.to_json v.ip_permissions)
       ; Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "GroupId", String.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "FromPort", Integer.to_json f)
       ; Aws.Util.option_map v.cidr_ip (fun f -> "CidrIp", String.to_json f)
       ])

let of_json j =
  { cidr_ip = Aws.Util.option_map (Aws.Json.lookup j "CidrIp") String.of_json
  ; from_port = Aws.Util.option_map (Aws.Json.lookup j "FromPort") Integer.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "GroupId") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
  ; ip_permissions =
      IpPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpPermissions"))
  ; ip_protocol = Aws.Util.option_map (Aws.Json.lookup j "IpProtocol") String.of_json
  ; source_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "SourceSecurityGroupName") String.of_json
  ; source_security_group_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "SourceSecurityGroupOwnerId") String.of_json
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "ToPort") Integer.of_json
  ; security_group_rule_ids =
      SecurityGroupRuleIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupRuleId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
