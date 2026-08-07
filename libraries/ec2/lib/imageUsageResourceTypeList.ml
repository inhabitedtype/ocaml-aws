type t = ImageUsageResourceType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageUsageResourceType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageUsageResourceType.to_query v
let to_json v = `List (List.map ImageUsageResourceType.to_json v)
let of_json j = Aws.Json.to_list ImageUsageResourceType.of_json j
