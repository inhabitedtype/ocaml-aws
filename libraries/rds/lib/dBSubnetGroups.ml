type t = DBSubnetGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBSubnetGroup.parse (Aws.Xml.members "DBSubnetGroup" xml))

let to_query v = Aws.Query.to_query_list DBSubnetGroup.to_query v
let to_json v = `List (List.map DBSubnetGroup.to_json v)
let of_json j = Aws.Json.to_list DBSubnetGroup.of_json j
