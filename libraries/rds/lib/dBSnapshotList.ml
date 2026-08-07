type t = DBSnapshot.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBSnapshot.parse (Aws.Xml.members "DBSnapshot" xml))

let to_query v = Aws.Query.to_query_list DBSnapshot.to_query v
let to_json v = `List (List.map DBSnapshot.to_json v)
let of_json j = Aws.Json.to_list DBSnapshot.of_json j
