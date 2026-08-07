type t = RecommendedActionUpdate.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RecommendedActionUpdate.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RecommendedActionUpdate.to_query v
let to_json v = `List (List.map RecommendedActionUpdate.to_json v)
let of_json j = Aws.Json.to_list RecommendedActionUpdate.of_json j
