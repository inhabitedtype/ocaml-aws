type t = NetworkCardInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NetworkCardInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkCardInfo.to_query v
let to_json v = `List (List.map NetworkCardInfo.to_json v)
let of_json j = Aws.Json.to_list NetworkCardInfo.of_json j
