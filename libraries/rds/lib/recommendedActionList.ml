type t = RecommendedAction.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RecommendedAction.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RecommendedAction.to_query v
let to_json v = `List (List.map RecommendedAction.to_json v)
let of_json j = Aws.Json.to_list RecommendedAction.of_json j
