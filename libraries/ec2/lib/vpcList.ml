type t = Vpc.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Vpc.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Vpc.to_query v
let to_json v = `List (List.map Vpc.to_json v)
let of_json j = Aws.Json.to_list Vpc.of_json j
