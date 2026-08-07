type t = ImageCriterion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageCriterion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageCriterion.to_query v
let to_json v = `List (List.map ImageCriterion.to_json v)
let of_json j = Aws.Json.to_list ImageCriterion.of_json j
