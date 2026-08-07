type t = InstanceNetworkInterface.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceNetworkInterface.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceNetworkInterface.to_query v
let to_json v = `List (List.map InstanceNetworkInterface.to_json v)
let of_json j = Aws.Json.to_list InstanceNetworkInterface.of_json j
