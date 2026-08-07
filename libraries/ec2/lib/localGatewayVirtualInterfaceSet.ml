type t = LocalGatewayVirtualInterface.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LocalGatewayVirtualInterface.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LocalGatewayVirtualInterface.to_query v
let to_json v = `List (List.map LocalGatewayVirtualInterface.to_json v)
let of_json j = Aws.Json.to_list LocalGatewayVirtualInterface.of_json j
