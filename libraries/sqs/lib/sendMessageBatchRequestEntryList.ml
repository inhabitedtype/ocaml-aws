type t = SendMessageBatchRequestEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SendMessageBatchRequestEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list SendMessageBatchRequestEntry.to_query v
let to_json v = `List (List.map SendMessageBatchRequestEntry.to_json v)
let of_json j = Aws.Json.to_list SendMessageBatchRequestEntry.of_json j
