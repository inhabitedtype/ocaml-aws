type t = LoadPermissionRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LoadPermissionRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LoadPermissionRequest.to_query v
let to_json v = `List (List.map LoadPermissionRequest.to_json v)
let of_json j = Aws.Json.to_list LoadPermissionRequest.of_json j
