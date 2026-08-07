type t = NewDhcpConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NewDhcpConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NewDhcpConfiguration.to_query v
let to_json v = `List (List.map NewDhcpConfiguration.to_json v)
let of_json j = Aws.Json.to_list NewDhcpConfiguration.of_json j
