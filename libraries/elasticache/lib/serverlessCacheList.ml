type t = ServerlessCache.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ServerlessCache.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ServerlessCache.to_query v
let to_json v = `List (List.map ServerlessCache.to_json v)
let of_json j = Aws.Json.to_list ServerlessCache.of_json j
