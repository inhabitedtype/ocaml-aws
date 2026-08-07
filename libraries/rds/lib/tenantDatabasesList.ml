type t = TenantDatabase.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TenantDatabase.parse (Aws.Xml.members "TenantDatabase" xml))

let to_query v = Aws.Query.to_query_list TenantDatabase.to_query v
let to_json v = `List (List.map TenantDatabase.to_json v)
let of_json j = Aws.Json.to_list TenantDatabase.of_json j
