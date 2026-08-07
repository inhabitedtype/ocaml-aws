open Aws.BaseTypes

type t =
  { rule_group_arn : String.t option
  ; rule_group_type : String.t option
  }

let make ?rule_group_arn ?rule_group_type () = { rule_group_arn; rule_group_type }

let parse xml =
  Some
    { rule_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "ruleGroupArn" xml) String.parse
    ; rule_group_type =
        Aws.Util.option_bind (Aws.Xml.member "ruleGroupType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_group_type (fun f ->
             Aws.Query.Pair ("RuleGroupType", String.to_query f))
       ; Aws.Util.option_map v.rule_group_arn (fun f ->
             Aws.Query.Pair ("RuleGroupArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_group_type (fun f ->
             "ruleGroupType", String.to_json f)
       ; Aws.Util.option_map v.rule_group_arn (fun f -> "ruleGroupArn", String.to_json f)
       ])

let of_json j =
  { rule_group_arn = Aws.Util.option_map (Aws.Json.lookup j "ruleGroupArn") String.of_json
  ; rule_group_type =
      Aws.Util.option_map (Aws.Json.lookup j "ruleGroupType") String.of_json
  }
