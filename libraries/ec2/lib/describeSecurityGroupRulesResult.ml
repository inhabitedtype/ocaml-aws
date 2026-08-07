open Aws.BaseTypes

type t =
  { security_group_rules : SecurityGroupRuleList.t
  ; next_token : String.t option
  }

let make ?(security_group_rules = []) ?next_token () =
  { security_group_rules; next_token }

let parse xml =
  Some
    { security_group_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupRuleSet" xml)
             SecurityGroupRuleList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupRuleSet"
              , SecurityGroupRuleList.to_query v.security_group_rules ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("securityGroupRuleSet", SecurityGroupRuleList.to_json v.security_group_rules)
       ])

let of_json j =
  { security_group_rules =
      SecurityGroupRuleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupRuleSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
