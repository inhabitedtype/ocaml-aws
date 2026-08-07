type t = NodeGroupConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NodeGroupConfiguration.parse (Aws.Xml.members "NodeGroupConfiguration" xml))

let to_query v = Aws.Query.to_query_list NodeGroupConfiguration.to_query v
let to_json v = `List (List.map NodeGroupConfiguration.to_json v)
let of_json j = Aws.Json.to_list NodeGroupConfiguration.of_json j
