type t = LogDeliveryConfigurationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LogDeliveryConfigurationRequest.parse
       (Aws.Xml.members "LogDeliveryConfigurationRequest" xml))

let to_query v = Aws.Query.to_query_list LogDeliveryConfigurationRequest.to_query v
let to_json v = `List (List.map LogDeliveryConfigurationRequest.to_json v)
let of_json j = Aws.Json.to_list LogDeliveryConfigurationRequest.of_json j
