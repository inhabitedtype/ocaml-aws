type t = NetworkInsightsPath.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NetworkInsightsPath.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkInsightsPath.to_query v
let to_json v = `List (List.map NetworkInsightsPath.to_json v)
let of_json j = Aws.Json.to_list NetworkInsightsPath.of_json j
