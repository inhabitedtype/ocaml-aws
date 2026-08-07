type t = NetworkAcl.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NetworkAcl.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkAcl.to_query v
let to_json v = `List (List.map NetworkAcl.to_json v)
let of_json j = Aws.Json.to_list NetworkAcl.of_json j
