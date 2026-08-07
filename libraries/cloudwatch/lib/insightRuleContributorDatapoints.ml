type t = InsightRuleContributorDatapoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InsightRuleContributorDatapoint.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InsightRuleContributorDatapoint.to_query v
let to_json v = `List (List.map InsightRuleContributorDatapoint.to_json v)
let of_json j = Aws.Json.to_list InsightRuleContributorDatapoint.of_json j
