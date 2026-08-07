open Aws.BaseTypes

type t =
  { security_group_rule_id : String.t
  ; security_group_rule : SecurityGroupRuleRequest.t option
  }

let make ~security_group_rule_id ?security_group_rule () =
  { security_group_rule_id; security_group_rule }

let parse xml =
  Some
    { security_group_rule_id =
        Aws.Xml.required
          "SecurityGroupRuleId"
          (Aws.Util.option_bind (Aws.Xml.member "SecurityGroupRuleId" xml) String.parse)
    ; security_group_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "SecurityGroupRule" xml)
          SecurityGroupRuleRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.security_group_rule (fun f ->
             Aws.Query.Pair ("SecurityGroupRule", SecurityGroupRuleRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupRuleId", String.to_query v.security_group_rule_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.security_group_rule (fun f ->
             "SecurityGroupRule", SecurityGroupRuleRequest.to_json f)
       ; Some ("SecurityGroupRuleId", String.to_json v.security_group_rule_id)
       ])

let of_json j =
  { security_group_rule_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupRuleId"))
  ; security_group_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "SecurityGroupRule")
        SecurityGroupRuleRequest.of_json
  }
