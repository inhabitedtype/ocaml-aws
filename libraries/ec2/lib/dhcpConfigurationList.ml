type t = DhcpConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DhcpConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DhcpConfiguration.to_query v
let to_json v = `List (List.map DhcpConfiguration.to_json v)
let of_json j = Aws.Json.to_list DhcpConfiguration.of_json j
