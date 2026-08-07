type t = GroupIdentifier.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map GroupIdentifier.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list GroupIdentifier.to_query v
let to_json v = `List (List.map GroupIdentifier.to_json v)
let of_json j = Aws.Json.to_list GroupIdentifier.of_json j
