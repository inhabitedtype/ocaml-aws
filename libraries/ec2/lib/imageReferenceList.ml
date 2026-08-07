type t = ImageReference.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageReference.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageReference.to_query v
let to_json v = `List (List.map ImageReference.to_json v)
let of_json j = Aws.Json.to_list ImageReference.of_json j
