type t = InsightRule.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InsightRule.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InsightRule.to_query v
let to_json v = `List (List.map InsightRule.to_json v)
let of_json j = Aws.Json.to_list InsightRule.of_json j
