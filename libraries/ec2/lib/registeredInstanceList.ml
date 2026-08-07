type t = RegisteredInstance.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RegisteredInstance.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RegisteredInstance.to_query v
let to_json v = `List (List.map RegisteredInstance.to_json v)
let of_json j = Aws.Json.to_list RegisteredInstance.of_json j
