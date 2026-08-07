type t = MacHost.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map MacHost.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list MacHost.to_query v
let to_json v = `List (List.map MacHost.to_json v)
let of_json j = Aws.Json.to_list MacHost.of_json j
