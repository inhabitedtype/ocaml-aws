type t = PrincipalIdFormat.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PrincipalIdFormat.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PrincipalIdFormat.to_query v
let to_json v = `List (List.map PrincipalIdFormat.to_json v)
let of_json j = Aws.Json.to_list PrincipalIdFormat.of_json j
