type t = DeleteMessageBatchRequestEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DeleteMessageBatchRequestEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DeleteMessageBatchRequestEntry.to_query v
let to_json v = `List (List.map DeleteMessageBatchRequestEntry.to_json v)
let of_json j = Aws.Json.to_list DeleteMessageBatchRequestEntry.of_json j
