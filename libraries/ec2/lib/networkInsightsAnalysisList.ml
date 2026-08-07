type t = NetworkInsightsAnalysis.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NetworkInsightsAnalysis.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkInsightsAnalysis.to_query v
let to_json v = `List (List.map NetworkInsightsAnalysis.to_json v)
let of_json j = Aws.Json.to_list NetworkInsightsAnalysis.of_json j
