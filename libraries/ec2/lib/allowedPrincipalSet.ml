type t = AllowedPrincipal.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AllowedPrincipal.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AllowedPrincipal.to_query v
let to_json v = `List (List.map AllowedPrincipal.to_json v)
let of_json j = Aws.Json.to_list AllowedPrincipal.of_json j
