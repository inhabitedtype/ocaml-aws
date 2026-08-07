type t = RuleGroupTypePair.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RuleGroupTypePair.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RuleGroupTypePair.to_query v
let to_json v = `List (List.map RuleGroupTypePair.to_json v)
let of_json j = Aws.Json.to_list RuleGroupTypePair.of_json j
