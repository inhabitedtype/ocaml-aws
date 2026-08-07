type t = PendingLogDeliveryConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PendingLogDeliveryConfiguration.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PendingLogDeliveryConfiguration.to_query v
let to_json v = `List (List.map PendingLogDeliveryConfiguration.to_json v)
let of_json j = Aws.Json.to_list PendingLogDeliveryConfiguration.of_json j
