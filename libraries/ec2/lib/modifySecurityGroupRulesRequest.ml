open Aws.BaseTypes

type t =
  { group_id : String.t
  ; security_group_rules : SecurityGroupRuleUpdateList.t
  ; dry_run : Boolean.t option
  }

let make ~group_id ~security_group_rules ?dry_run () =
  { group_id; security_group_rules; dry_run }

let parse xml =
  Some
    { group_id =
        Aws.Xml.required
          "GroupId"
          (Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) String.parse)
    ; security_group_rules =
        Aws.Xml.required
          "SecurityGroupRule"
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupRule" xml)
             SecurityGroupRuleUpdateList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupRule"
              , SecurityGroupRuleUpdateList.to_query v.security_group_rules ))
       ; Some (Aws.Query.Pair ("GroupId", String.to_query v.group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "SecurityGroupRule"
           , SecurityGroupRuleUpdateList.to_json v.security_group_rules )
       ; Some ("GroupId", String.to_json v.group_id)
       ])

let of_json j =
  { group_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupId"))
  ; security_group_rules =
      SecurityGroupRuleUpdateList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupRule"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
