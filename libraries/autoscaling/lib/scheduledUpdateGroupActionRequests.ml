type t = ScheduledUpdateGroupActionRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ScheduledUpdateGroupActionRequest.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ScheduledUpdateGroupActionRequest.to_query v
let to_json v = `List (List.map ScheduledUpdateGroupActionRequest.to_json v)
let of_json j = Aws.Json.to_list ScheduledUpdateGroupActionRequest.of_json j
