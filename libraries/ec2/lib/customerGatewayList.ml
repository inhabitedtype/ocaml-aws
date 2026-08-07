type t = CustomerGateway.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CustomerGateway.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CustomerGateway.to_query v
let to_json v = `List (List.map CustomerGateway.to_json v)
let of_json j = Aws.Json.to_list CustomerGateway.of_json j
