open Aws.BaseTypes

type t =
  { return : Boolean.t option
  ; security_group_rules : SecurityGroupRuleList.t
  }

let make ?return ?(security_group_rules = []) () = { return; security_group_rules }

let parse xml =
  Some
    { return = Aws.Util.option_bind (Aws.Xml.member "return" xml) Boolean.parse
    ; security_group_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupRuleSet" xml)
             SecurityGroupRuleList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecurityGroupRuleSet"
              , SecurityGroupRuleList.to_query v.security_group_rules ))
       ; Aws.Util.option_map v.return (fun f ->
             Aws.Query.Pair ("Return", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("securityGroupRuleSet", SecurityGroupRuleList.to_json v.security_group_rules)
       ; Aws.Util.option_map v.return (fun f -> "return", Boolean.to_json f)
       ])

let of_json j =
  { return = Aws.Util.option_map (Aws.Json.lookup j "return") Boolean.of_json
  ; security_group_rules =
      SecurityGroupRuleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupRuleSet"))
  }
