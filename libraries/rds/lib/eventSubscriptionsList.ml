type t = EventSubscription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map EventSubscription.parse (Aws.Xml.members "EventSubscription" xml))

let to_query v = Aws.Query.to_query_list EventSubscription.to_query v
let to_json v = `List (List.map EventSubscription.to_json v)
let of_json j = Aws.Json.to_list EventSubscription.of_json j
