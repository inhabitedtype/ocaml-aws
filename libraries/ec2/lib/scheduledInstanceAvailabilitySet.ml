type t = ScheduledInstanceAvailability.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ScheduledInstanceAvailability.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ScheduledInstanceAvailability.to_query v
let to_json v = `List (List.map ScheduledInstanceAvailability.to_json v)
let of_json j = Aws.Json.to_list ScheduledInstanceAvailability.of_json j
