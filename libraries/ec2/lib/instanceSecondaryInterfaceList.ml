type t = InstanceSecondaryInterface.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceSecondaryInterface.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceSecondaryInterface.to_query v
let to_json v = `List (List.map InstanceSecondaryInterface.to_json v)
let of_json j = Aws.Json.to_list InstanceSecondaryInterface.of_json j
