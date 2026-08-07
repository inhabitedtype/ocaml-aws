type t = SourceEventCategory.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SourceEventCategory.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list SourceEventCategory.to_query v
let to_json v = `List (List.map SourceEventCategory.to_json v)
let of_json j = Aws.Json.to_list SourceEventCategory.of_json j
