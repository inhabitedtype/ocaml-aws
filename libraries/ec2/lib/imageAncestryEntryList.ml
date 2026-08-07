type t = ImageAncestryEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageAncestryEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageAncestryEntry.to_query v
let to_json v = `List (List.map ImageAncestryEntry.to_json v)
let of_json j = Aws.Json.to_list ImageAncestryEntry.of_json j
