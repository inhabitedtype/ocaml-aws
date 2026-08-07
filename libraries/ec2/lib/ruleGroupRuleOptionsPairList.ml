type t = RuleGroupRuleOptionsPair.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RuleGroupRuleOptionsPair.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RuleGroupRuleOptionsPair.to_query v
let to_json v = `List (List.map RuleGroupRuleOptionsPair.to_json v)
let of_json j = Aws.Json.to_list RuleGroupRuleOptionsPair.of_json j
