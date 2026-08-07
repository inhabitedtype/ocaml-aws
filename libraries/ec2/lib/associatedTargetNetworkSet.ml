type t = AssociatedTargetNetwork.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AssociatedTargetNetwork.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AssociatedTargetNetwork.to_query v
let to_json v = `List (List.map AssociatedTargetNetwork.to_json v)
let of_json j = Aws.Json.to_list AssociatedTargetNetwork.of_json j
