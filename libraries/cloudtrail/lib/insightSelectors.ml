type t = InsightSelector.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InsightSelector.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InsightSelector.to_query v
let to_json v = `List (List.map InsightSelector.to_json v)
let of_json j = Aws.Json.to_list InsightSelector.of_json j
