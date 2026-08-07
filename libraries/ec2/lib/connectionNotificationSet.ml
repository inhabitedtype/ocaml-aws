type t = ConnectionNotification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ConnectionNotification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ConnectionNotification.to_query v
let to_json v = `List (List.map ConnectionNotification.to_json v)
let of_json j = Aws.Json.to_list ConnectionNotification.of_json j
