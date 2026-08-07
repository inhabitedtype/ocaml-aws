type t = SecondaryInterface.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SecondaryInterface.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecondaryInterface.to_query v
let to_json v = `List (List.map SecondaryInterface.to_json v)
let of_json j = Aws.Json.to_list SecondaryInterface.of_json j
