type t = TypeSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TypeSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TypeSummary.to_query v
let to_json v = `List (List.map TypeSummary.to_json v)
let of_json j = Aws.Json.to_list TypeSummary.of_json j
