type t = Subscription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Subscription.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Subscription.to_query v
let to_json v = `List (List.map Subscription.to_json v)
let of_json j = Aws.Json.to_list Subscription.of_json j
