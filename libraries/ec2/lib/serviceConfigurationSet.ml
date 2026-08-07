type t = ServiceConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ServiceConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ServiceConfiguration.to_query v
let to_json v = `List (List.map ServiceConfiguration.to_json v)
let of_json j = Aws.Json.to_list ServiceConfiguration.of_json j
