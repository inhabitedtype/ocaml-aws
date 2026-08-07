type t = RemovePrefixListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RemovePrefixListEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RemovePrefixListEntry.to_query v
let to_json v = `List (List.map RemovePrefixListEntry.to_json v)
let of_json j = Aws.Json.to_list RemovePrefixListEntry.of_json j
