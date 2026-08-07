type t = NetworkType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NetworkType.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list NetworkType.to_query v
let to_json v = `List (List.map NetworkType.to_json v)
let of_json j = Aws.Json.to_list NetworkType.of_json j
