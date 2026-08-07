type t = Range.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Range.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Range.to_query v
let to_json v = `List (List.map Range.to_json v)
let of_json j = Aws.Json.to_list Range.of_json j
