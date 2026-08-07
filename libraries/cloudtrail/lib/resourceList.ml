type t = Resource.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Resource.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Resource.to_query v
let to_json v = `List (List.map Resource.to_json v)
let of_json j = Aws.Json.to_list Resource.of_json j
