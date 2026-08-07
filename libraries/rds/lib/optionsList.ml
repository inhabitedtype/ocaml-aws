type t = Option.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Option.parse (Aws.Xml.members "Option" xml))
let to_query v = Aws.Query.to_query_list Option.to_query v
let to_json v = `List (List.map Option.to_json v)
let of_json j = Aws.Json.to_list Option.of_json j
