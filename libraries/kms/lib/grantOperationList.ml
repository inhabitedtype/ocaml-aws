type t = GrantOperation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map GrantOperation.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list GrantOperation.to_query v
let to_json v = `List (List.map GrantOperation.to_json v)
let of_json j = Aws.Json.to_list GrantOperation.of_json j
