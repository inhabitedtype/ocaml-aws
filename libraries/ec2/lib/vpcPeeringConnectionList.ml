type t = VpcPeeringConnection.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcPeeringConnection.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcPeeringConnection.to_query v
let to_json v = `List (List.map VpcPeeringConnection.to_json v)
let of_json j = Aws.Json.to_list VpcPeeringConnection.of_json j
