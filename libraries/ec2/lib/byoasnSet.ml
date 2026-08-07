type t = Byoasn.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Byoasn.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Byoasn.to_query v
let to_json v = `List (List.map Byoasn.to_json v)
let of_json j = Aws.Json.to_list Byoasn.of_json j
