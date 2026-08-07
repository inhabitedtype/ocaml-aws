type t = ReservedDBInstancesOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ReservedDBInstancesOffering.parse
       (Aws.Xml.members "ReservedDBInstancesOffering" xml))

let to_query v = Aws.Query.to_query_list ReservedDBInstancesOffering.to_query v
let to_json v = `List (List.map ReservedDBInstancesOffering.to_json v)
let of_json j = Aws.Json.to_list ReservedDBInstancesOffering.of_json j
