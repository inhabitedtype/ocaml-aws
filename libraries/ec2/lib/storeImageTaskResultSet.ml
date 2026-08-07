type t = StoreImageTaskResult.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StoreImageTaskResult.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list StoreImageTaskResult.to_query v
let to_json v = `List (List.map StoreImageTaskResult.to_json v)
let of_json j = Aws.Json.to_list StoreImageTaskResult.of_json j
