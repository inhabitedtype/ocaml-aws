type t = Stack.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Stack.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Stack.to_query v
let to_json v = `List (List.map Stack.to_json v)
let of_json j = Aws.Json.to_list Stack.of_json j
