type t = User.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map User.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list User.to_query v
let to_json v = `List (List.map User.to_json v)
let of_json j = Aws.Json.to_list User.of_json j
