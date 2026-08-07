type t = DeletableItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DeletableItem.parse (Aws.Xml.members "Item" xml))

let to_query v = Aws.Query.to_query_list DeletableItem.to_query v
let to_json v = `List (List.map DeletableItem.to_json v)
let of_json j = Aws.Json.to_list DeletableItem.of_json j
