type t = NatGateway.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NatGateway.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NatGateway.to_query v
let to_json v = `List (List.map NatGateway.to_json v)
let of_json j = Aws.Json.to_list NatGateway.of_json j
