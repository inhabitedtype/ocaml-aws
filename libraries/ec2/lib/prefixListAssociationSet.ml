type t = PrefixListAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PrefixListAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PrefixListAssociation.to_query v
let to_json v = `List (List.map PrefixListAssociation.to_json v)
let of_json j = Aws.Json.to_list PrefixListAssociation.of_json j
