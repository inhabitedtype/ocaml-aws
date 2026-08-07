type t = PrefixList.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PrefixList.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PrefixList.to_query v
let to_json v = `List (List.map PrefixList.to_json v)
let of_json j = Aws.Json.to_list PrefixList.of_json j
