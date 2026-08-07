type t = ListMessageMoveTasksResultEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ListMessageMoveTasksResultEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ListMessageMoveTasksResultEntry.to_query v
let to_json v = `List (List.map ListMessageMoveTasksResultEntry.to_json v)
let of_json j = Aws.Json.to_list ListMessageMoveTasksResultEntry.of_json j
