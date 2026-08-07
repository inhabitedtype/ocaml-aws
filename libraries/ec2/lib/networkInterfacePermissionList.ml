type t = NetworkInterfacePermission.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NetworkInterfacePermission.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkInterfacePermission.to_query v
let to_json v = `List (List.map NetworkInterfacePermission.to_json v)
let of_json j = Aws.Json.to_list NetworkInterfacePermission.of_json j
