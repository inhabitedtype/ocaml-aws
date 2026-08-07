type t = RotationsListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RotationsListEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RotationsListEntry.to_query v
let to_json v = `List (List.map RotationsListEntry.to_json v)
let of_json j = Aws.Json.to_list RotationsListEntry.of_json j
