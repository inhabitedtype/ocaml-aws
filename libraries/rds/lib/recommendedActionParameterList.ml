type t = RecommendedActionParameter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RecommendedActionParameter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RecommendedActionParameter.to_query v
let to_json v = `List (List.map RecommendedActionParameter.to_json v)
let of_json j = Aws.Json.to_list RecommendedActionParameter.of_json j
