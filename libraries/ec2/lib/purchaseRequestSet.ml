type t = PurchaseRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PurchaseRequest.parse (Aws.Xml.members "PurchaseRequest" xml))

let to_query v = Aws.Query.to_query_list PurchaseRequest.to_query v
let to_json v = `List (List.map PurchaseRequest.to_json v)
let of_json j = Aws.Json.to_list PurchaseRequest.of_json j
