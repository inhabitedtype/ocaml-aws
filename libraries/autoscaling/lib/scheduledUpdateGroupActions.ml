type t = ScheduledUpdateGroupAction.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ScheduledUpdateGroupAction.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ScheduledUpdateGroupAction.to_query v
let to_json v = `List (List.map ScheduledUpdateGroupAction.to_json v)
let of_json j = Aws.Json.to_list ScheduledUpdateGroupAction.of_json j
