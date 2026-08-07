type t = AssignedPrivateIpAddress.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AssignedPrivateIpAddress.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AssignedPrivateIpAddress.to_query v
let to_json v = `List (List.map AssignedPrivateIpAddress.to_json v)
let of_json j = Aws.Json.to_list AssignedPrivateIpAddress.of_json j
