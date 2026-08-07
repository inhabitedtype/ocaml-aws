type t = NetworkAclEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NetworkAclEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkAclEntry.to_query v
let to_json v = `List (List.map NetworkAclEntry.to_json v)
let of_json j = Aws.Json.to_list NetworkAclEntry.of_json j
