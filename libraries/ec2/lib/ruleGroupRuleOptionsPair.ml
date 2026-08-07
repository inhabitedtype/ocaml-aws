open Aws.BaseTypes

type t =
  { rule_group_arn : String.t option
  ; rule_options : RuleOptionList.t
  }

let make ?rule_group_arn ?(rule_options = []) () = { rule_group_arn; rule_options }

let parse xml =
  Some
    { rule_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "ruleGroupArn" xml) String.parse
    ; rule_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ruleOptionSet" xml) RuleOptionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("RuleOptionSet", RuleOptionList.to_query v.rule_options))
       ; Aws.Util.option_map v.rule_group_arn (fun f ->
             Aws.Query.Pair ("RuleGroupArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ruleOptionSet", RuleOptionList.to_json v.rule_options)
       ; Aws.Util.option_map v.rule_group_arn (fun f -> "ruleGroupArn", String.to_json f)
       ])

let of_json j =
  { rule_group_arn = Aws.Util.option_map (Aws.Json.lookup j "ruleGroupArn") String.of_json
  ; rule_options =
      RuleOptionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleOptionSet"))
  }
