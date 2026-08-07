type t = ImageUsageResourceTypeRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImageUsageResourceTypeRequest.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ImageUsageResourceTypeRequest.to_query v
let to_json v = `List (List.map ImageUsageResourceTypeRequest.to_json v)
let of_json j = Aws.Json.to_list ImageUsageResourceTypeRequest.of_json j
