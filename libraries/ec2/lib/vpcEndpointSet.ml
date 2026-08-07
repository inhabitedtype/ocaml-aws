type t = VpcEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcEndpoint.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcEndpoint.to_query v
let to_json v = `List (List.map VpcEndpoint.to_json v)
let of_json j = Aws.Json.to_list VpcEndpoint.of_json j
