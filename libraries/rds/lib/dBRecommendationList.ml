type t = DBRecommendation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBRecommendation.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DBRecommendation.to_query v
let to_json v = `List (List.map DBRecommendation.to_json v)
let of_json j = Aws.Json.to_list DBRecommendation.of_json j
