type t = TagKeyOnly.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TagKeyOnly.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TagKeyOnly.to_query v
let to_json v = `List (List.map TagKeyOnly.to_json v)
let of_json j = Aws.Json.to_list TagKeyOnly.of_json j
