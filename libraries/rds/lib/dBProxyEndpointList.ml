type t = DBProxyEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBProxyEndpoint.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DBProxyEndpoint.to_query v
let to_json v = `List (List.map DBProxyEndpoint.to_json v)
let of_json j = Aws.Json.to_list DBProxyEndpoint.of_json j
