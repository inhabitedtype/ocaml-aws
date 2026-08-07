type t = AccessScopeAnalysisFinding.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AccessScopeAnalysisFinding.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AccessScopeAnalysisFinding.to_query v
let to_json v = `List (List.map AccessScopeAnalysisFinding.to_json v)
let of_json j = Aws.Json.to_list AccessScopeAnalysisFinding.of_json j
