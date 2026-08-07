type t = ResourcePendingMaintenanceActions.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ResourcePendingMaintenanceActions.parse
       (Aws.Xml.members "ResourcePendingMaintenanceActions" xml))

let to_query v = Aws.Query.to_query_list ResourcePendingMaintenanceActions.to_query v
let to_json v = `List (List.map ResourcePendingMaintenanceActions.to_json v)
let of_json j = Aws.Json.to_list ResourcePendingMaintenanceActions.of_json j
