type t = AssociatedRole.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AssociatedRole.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AssociatedRole.to_query v
let to_json v = `List (List.map AssociatedRole.to_json v)
let of_json j = Aws.Json.to_list AssociatedRole.of_json j
