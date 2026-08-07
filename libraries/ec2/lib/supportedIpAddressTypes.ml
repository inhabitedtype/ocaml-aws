type t = ServiceConnectivityType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ServiceConnectivityType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ServiceConnectivityType.to_query v
let to_json v = `List (List.map ServiceConnectivityType.to_json v)
let of_json j = Aws.Json.to_list ServiceConnectivityType.of_json j
