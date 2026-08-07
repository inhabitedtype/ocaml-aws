type t = ServiceDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ServiceDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ServiceDetail.to_query v
let to_json v = `List (List.map ServiceDetail.to_json v)
let of_json j = Aws.Json.to_list ServiceDetail.of_json j
