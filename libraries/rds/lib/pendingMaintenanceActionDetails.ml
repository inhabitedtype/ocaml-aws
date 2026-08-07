type t = PendingMaintenanceAction.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       PendingMaintenanceAction.parse
       (Aws.Xml.members "PendingMaintenanceAction" xml))

let to_query v = Aws.Query.to_query_list PendingMaintenanceAction.to_query v
let to_json v = `List (List.map PendingMaintenanceAction.to_json v)
let of_json j = Aws.Json.to_list PendingMaintenanceAction.of_json j
