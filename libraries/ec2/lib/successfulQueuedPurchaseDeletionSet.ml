type t = SuccessfulQueuedPurchaseDeletion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SuccessfulQueuedPurchaseDeletion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SuccessfulQueuedPurchaseDeletion.to_query v
let to_json v = `List (List.map SuccessfulQueuedPurchaseDeletion.to_json v)
let of_json j = Aws.Json.to_list SuccessfulQueuedPurchaseDeletion.of_json j
