type t = ImageUsageResourceTypeOptionRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImageUsageResourceTypeOptionRequest.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ImageUsageResourceTypeOptionRequest.to_query v
let to_json v = `List (List.map ImageUsageResourceTypeOptionRequest.to_json v)
let of_json j = Aws.Json.to_list ImageUsageResourceTypeOptionRequest.of_json j
