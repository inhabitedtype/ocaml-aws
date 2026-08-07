type t = EgressOnlyInternetGateway.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map EgressOnlyInternetGateway.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list EgressOnlyInternetGateway.to_query v
let to_json v = `List (List.map EgressOnlyInternetGateway.to_json v)
let of_json j = Aws.Json.to_list EgressOnlyInternetGateway.of_json j
