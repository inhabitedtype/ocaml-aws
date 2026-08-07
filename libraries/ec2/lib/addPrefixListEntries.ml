type t = AddPrefixListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AddPrefixListEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AddPrefixListEntry.to_query v
let to_json v = `List (List.map AddPrefixListEntry.to_json v)
let of_json j = Aws.Json.to_list AddPrefixListEntry.of_json j
