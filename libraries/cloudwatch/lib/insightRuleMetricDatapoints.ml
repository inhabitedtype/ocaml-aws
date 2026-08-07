type t = InsightRuleMetricDatapoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InsightRuleMetricDatapoint.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InsightRuleMetricDatapoint.to_query v
let to_json v = `List (List.map InsightRuleMetricDatapoint.to_json v)
let of_json j = Aws.Json.to_list InsightRuleMetricDatapoint.of_json j
