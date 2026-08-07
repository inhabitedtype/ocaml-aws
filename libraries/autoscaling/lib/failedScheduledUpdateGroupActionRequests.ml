type t = FailedScheduledUpdateGroupActionRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       FailedScheduledUpdateGroupActionRequest.parse
       (Aws.Xml.members "member" xml))

let to_query v =
  Aws.Query.to_query_list FailedScheduledUpdateGroupActionRequest.to_query v

let to_json v = `List (List.map FailedScheduledUpdateGroupActionRequest.to_json v)
let of_json j = Aws.Json.to_list FailedScheduledUpdateGroupActionRequest.of_json j
