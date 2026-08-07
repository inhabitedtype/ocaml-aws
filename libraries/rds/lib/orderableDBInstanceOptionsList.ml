type t = OrderableDBInstanceOption.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       OrderableDBInstanceOption.parse
       (Aws.Xml.members "OrderableDBInstanceOption" xml))

let to_query v = Aws.Query.to_query_list OrderableDBInstanceOption.to_query v
let to_json v = `List (List.map OrderableDBInstanceOption.to_json v)
let of_json j = Aws.Json.to_list OrderableDBInstanceOption.of_json j
