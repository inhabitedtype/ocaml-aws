type t = DBProxy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBProxy.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DBProxy.to_query v
let to_json v = `List (List.map DBProxy.to_json v)
let of_json j = Aws.Json.to_list DBProxy.of_json j
