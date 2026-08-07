type t = DhcpOptions.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DhcpOptions.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DhcpOptions.to_query v
let to_json v = `List (List.map DhcpOptions.to_json v)
let of_json j = Aws.Json.to_list DhcpOptions.of_json j
