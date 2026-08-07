type t = ReservedInstancesOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReservedInstancesOffering.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ReservedInstancesOffering.to_query v
let to_json v = `List (List.map ReservedInstancesOffering.to_json v)
let of_json j = Aws.Json.to_list ReservedInstancesOffering.of_json j
