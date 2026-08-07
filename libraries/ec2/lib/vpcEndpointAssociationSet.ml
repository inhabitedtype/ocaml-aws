type t = VpcEndpointAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcEndpointAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcEndpointAssociation.to_query v
let to_json v = `List (List.map VpcEndpointAssociation.to_json v)
let of_json j = Aws.Json.to_list VpcEndpointAssociation.of_json j
