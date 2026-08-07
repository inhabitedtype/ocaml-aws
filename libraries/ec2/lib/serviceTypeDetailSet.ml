type t = ServiceTypeDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ServiceTypeDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ServiceTypeDetail.to_query v
let to_json v = `List (List.map ServiceTypeDetail.to_json v)
let of_json j = Aws.Json.to_list ServiceTypeDetail.of_json j
