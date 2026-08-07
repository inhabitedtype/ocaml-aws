type t = InternetGatewayAttachment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InternetGatewayAttachment.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InternetGatewayAttachment.to_query v
let to_json v = `List (List.map InternetGatewayAttachment.to_json v)
let of_json j = Aws.Json.to_list InternetGatewayAttachment.of_json j
