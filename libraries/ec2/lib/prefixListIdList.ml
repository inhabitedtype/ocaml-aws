type t = PrefixListId.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PrefixListId.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PrefixListId.to_query v
let to_json v = `List (List.map PrefixListId.to_json v)
let of_json j = Aws.Json.to_list PrefixListId.of_json j
