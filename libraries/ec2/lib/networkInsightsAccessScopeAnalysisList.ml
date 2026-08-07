type t = NetworkInsightsAccessScopeAnalysis.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NetworkInsightsAccessScopeAnalysis.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkInsightsAccessScopeAnalysis.to_query v
let to_json v = `List (List.map NetworkInsightsAccessScopeAnalysis.to_json v)
let of_json j = Aws.Json.to_list NetworkInsightsAccessScopeAnalysis.of_json j
