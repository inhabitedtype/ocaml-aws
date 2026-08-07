type t = KeyListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map KeyListEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list KeyListEntry.to_query v
let to_json v = `List (List.map KeyListEntry.to_json v)
let of_json j = Aws.Json.to_list KeyListEntry.of_json j
