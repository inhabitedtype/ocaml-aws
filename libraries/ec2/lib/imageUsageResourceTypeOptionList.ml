type t = ImageUsageResourceTypeOption.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImageUsageResourceTypeOption.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageUsageResourceTypeOption.to_query v
let to_json v = `List (List.map ImageUsageResourceTypeOption.to_json v)
let of_json j = Aws.Json.to_list ImageUsageResourceTypeOption.of_json j
