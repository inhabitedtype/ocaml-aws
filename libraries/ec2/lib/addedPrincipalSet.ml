type t = AddedPrincipal.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AddedPrincipal.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AddedPrincipal.to_query v
let to_json v = `List (List.map AddedPrincipal.to_json v)
let of_json j = Aws.Json.to_list AddedPrincipal.of_json j
