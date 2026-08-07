type t = VirtualizationType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VirtualizationType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VirtualizationType.to_query v
let to_json v = `List (List.map VirtualizationType.to_json v)
let of_json j = Aws.Json.to_list VirtualizationType.of_json j
