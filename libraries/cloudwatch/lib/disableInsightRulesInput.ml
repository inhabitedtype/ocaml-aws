type t = { rule_names : InsightRuleNames.t }

let make ~rule_names () = { rule_names }

let parse xml =
  Some
    { rule_names =
        Aws.Xml.required
          "RuleNames"
          (Aws.Util.option_bind (Aws.Xml.member "RuleNames" xml) InsightRuleNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("RuleNames.member", InsightRuleNames.to_query v.rule_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("RuleNames", InsightRuleNames.to_json v.rule_names) ])

let of_json j =
  { rule_names =
      InsightRuleNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleNames"))
  }
