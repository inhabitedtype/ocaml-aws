type t = Limit.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Limit.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Limit.to_query v
let to_json v = `List (List.map Limit.to_json v)
let of_json j = Aws.Json.to_list Limit.of_json j
