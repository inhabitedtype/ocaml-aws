type t = NodeSnapshot.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NodeSnapshot.parse (Aws.Xml.members "NodeSnapshot" xml))

let to_query v = Aws.Query.to_query_list NodeSnapshot.to_query v
let to_json v = `List (List.map NodeSnapshot.to_json v)
let of_json j = Aws.Json.to_list NodeSnapshot.of_json j
