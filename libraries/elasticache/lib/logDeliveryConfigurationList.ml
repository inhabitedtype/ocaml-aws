type t = LogDeliveryConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LogDeliveryConfiguration.parse
       (Aws.Xml.members "LogDeliveryConfiguration" xml))

let to_query v = Aws.Query.to_query_list LogDeliveryConfiguration.to_query v
let to_json v = `List (List.map LogDeliveryConfiguration.to_json v)
let of_json j = Aws.Json.to_list LogDeliveryConfiguration.of_json j
