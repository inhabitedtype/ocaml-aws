type t = VpcEndpointConnection.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcEndpointConnection.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcEndpointConnection.to_query v
let to_json v = `List (List.map VpcEndpointConnection.to_json v)
let of_json j = Aws.Json.to_list VpcEndpointConnection.of_json j
