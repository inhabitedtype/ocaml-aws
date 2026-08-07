type t = PropertyDifference.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PropertyDifference.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PropertyDifference.to_query v
let to_json v = `List (List.map PropertyDifference.to_json v)
let of_json j = Aws.Json.to_list PropertyDifference.of_json j
