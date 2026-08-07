type t = PrefixListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PrefixListEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PrefixListEntry.to_query v
let to_json v = `List (List.map PrefixListEntry.to_json v)
let of_json j = Aws.Json.to_list PrefixListEntry.of_json j
