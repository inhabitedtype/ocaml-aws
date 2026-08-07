type t = CacheNodeUpdateStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CacheNodeUpdateStatus.parse (Aws.Xml.members "CacheNodeUpdateStatus" xml))

let to_query v = Aws.Query.to_query_list CacheNodeUpdateStatus.to_query v
let to_json v = `List (List.map CacheNodeUpdateStatus.to_json v)
let of_json j = Aws.Json.to_list CacheNodeUpdateStatus.of_json j
