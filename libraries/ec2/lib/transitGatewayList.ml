type t = TransitGateway.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TransitGateway.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGateway.to_query v
let to_json v = `List (List.map TransitGateway.to_json v)
let of_json j = Aws.Json.to_list TransitGateway.of_json j
