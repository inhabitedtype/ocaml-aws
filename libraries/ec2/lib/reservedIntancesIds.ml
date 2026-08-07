type t = ReservedInstancesId.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ReservedInstancesId.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ReservedInstancesId.to_query v
let to_json v = `List (List.map ReservedInstancesId.to_json v)
let of_json j = Aws.Json.to_list ReservedInstancesId.of_json j
