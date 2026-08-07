type t = ServiceUpdate.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ServiceUpdate.parse (Aws.Xml.members "ServiceUpdate" xml))

let to_query v = Aws.Query.to_query_list ServiceUpdate.to_query v
let to_json v = `List (List.map ServiceUpdate.to_json v)
let of_json j = Aws.Json.to_list ServiceUpdate.of_json j
