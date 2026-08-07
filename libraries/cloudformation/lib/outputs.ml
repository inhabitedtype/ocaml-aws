type t = Output.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Output.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Output.to_query v
let to_json v = `List (List.map Output.to_json v)
let of_json j = Aws.Json.to_list Output.of_json j
