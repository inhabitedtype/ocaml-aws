type t = ChangeMessageVisibilityBatchRequestEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ChangeMessageVisibilityBatchRequestEntry.parse
       (Aws.Xml.members "member" xml))

let to_query v =
  Aws.Query.to_query_list ChangeMessageVisibilityBatchRequestEntry.to_query v

let to_json v = `List (List.map ChangeMessageVisibilityBatchRequestEntry.to_json v)
let of_json j = Aws.Json.to_list ChangeMessageVisibilityBatchRequestEntry.of_json j
