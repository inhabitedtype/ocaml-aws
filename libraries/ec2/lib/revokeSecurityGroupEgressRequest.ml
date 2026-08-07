open Aws.BaseTypes

type t =
  { security_group_rule_ids : SecurityGroupRuleIdList.t
  ; dry_run : Boolean.t option
  ; group_id : String.t
  ; source_security_group_name : String.t option
  ; source_security_group_owner_id : String.t option
  ; ip_protocol : String.t option
  ; from_port : Integer.t option
  ; to_port : Integer.t option
  ; cidr_ip : String.t option
  ; ip_permissions : IpPermissionList.t
  }

let make
    ?(security_group_rule_ids = [])
    ?dry_run
    ~group_id
    ?source_security_group_name
    ?source_security_group_owner_id
    ?ip_protocol
    ?from_port
    ?to_port
    ?cidr_ip
    ?(ip_permissions = [])
    () =
  { security_group_rule_ids
  ; dry_run
  ; group_id
  ; source_security_group_name
  ; source_security_group_owner_id
  ; ip_protocol
  ; from_port
  ; to_port
  ; cidr_ip
  ; ip_permissions
  }

let parse xml =
  Some
    { security_group_rule_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupRuleId" xml)
             SecurityGroupRuleIdList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; group_id =
        Aws.Xml.required
          "groupId"
          (Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse)
    ; source_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "sourceSecurityGroupName" xml) String.parse
    ; source_security_group_owner_id =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceSecurityGroupOwnerId" xml)
          String.parse
    ; ip_protocol = Aws.Util.option_bind (Aws.Xml.member "ipProtocol" xml) String.parse
    ; from_port = Aws.Util.option_bind (Aws.Xml.member "fromPort" xml) Integer.parse
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "toPort" xml) Integer.parse
    ; cidr_ip = Aws.Util.option_bind (Aws.Xml.member "cidrIp" xml) String.parse
    ; ip_permissions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipPermissions" xml)
             IpPermissionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("IpPermissions", IpPermissionList.to_query v.ip_permissions))
       ; Aws.Util.option_map v.cidr_ip (fun f ->
             Aws.Query.Pair ("CidrIp", String.to_query f))
       ; Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ; Aws.Util.option_map v.ip_protocol (fun f ->
             Aws.Query.Pair ("IpProtocol", String.to_query f))
       ; Aws.Util.option_map v.source_security_group_owner_id (fun f ->
             Aws.Query.Pair ("SourceSecurityGroupOwnerId", String.to_query f))
       ; Aws.Util.option_map v.source_security_group_name (fun f ->
             Aws.Query.Pair ("SourceSecurityGroupName", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupId", String.to_query v.group_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupRuleId"
              , SecurityGroupRuleIdList.to_query v.security_group_rule_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ipPermissions", IpPermissionList.to_json v.ip_permissions)
       ; Aws.Util.option_map v.cidr_ip (fun f -> "cidrIp", String.to_json f)
       ; Aws.Util.option_map v.to_port (fun f -> "toPort", Integer.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "fromPort", Integer.to_json f)
       ; Aws.Util.option_map v.ip_protocol (fun f -> "ipProtocol", String.to_json f)
       ; Aws.Util.option_map v.source_security_group_owner_id (fun f ->
             "sourceSecurityGroupOwnerId", String.to_json f)
       ; Aws.Util.option_map v.source_security_group_name (fun f ->
             "sourceSecurityGroupName", String.to_json f)
       ; Some ("groupId", String.to_json v.group_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some
           ( "SecurityGroupRuleId"
           , SecurityGroupRuleIdList.to_json v.security_group_rule_ids )
       ])

let of_json j =
  { security_group_rule_ids =
      SecurityGroupRuleIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupRuleId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; group_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupId"))
  ; source_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "sourceSecurityGroupName") String.of_json
  ; source_security_group_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceSecurityGroupOwnerId") String.of_json
  ; ip_protocol = Aws.Util.option_map (Aws.Json.lookup j "ipProtocol") String.of_json
  ; from_port = Aws.Util.option_map (Aws.Json.lookup j "fromPort") Integer.of_json
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "toPort") Integer.of_json
  ; cidr_ip = Aws.Util.option_map (Aws.Json.lookup j "cidrIp") String.of_json
  ; ip_permissions =
      IpPermissionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipPermissions"))
  }
