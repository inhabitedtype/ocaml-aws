type t = NotificationConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NotificationConfiguration.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list NotificationConfiguration.to_query v
let to_json v = `List (List.map NotificationConfiguration.to_json v)
let of_json j = Aws.Json.to_list NotificationConfiguration.of_json j
