type t = ServiceUpdateStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ServiceUpdateStatus.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ServiceUpdateStatus.to_query v
let to_json v = `List (List.map ServiceUpdateStatus.to_json v)
let of_json j = Aws.Json.to_list ServiceUpdateStatus.of_json j
