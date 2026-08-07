type t = NetworkInterface.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NetworkInterface.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkInterface.to_query v
let to_json v = `List (List.map NetworkInterface.to_json v)
let of_json j = Aws.Json.to_list NetworkInterface.of_json j
