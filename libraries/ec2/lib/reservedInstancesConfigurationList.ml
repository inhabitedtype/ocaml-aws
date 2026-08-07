type t = ReservedInstancesConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReservedInstancesConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ReservedInstancesConfiguration.to_query v
let to_json v = `List (List.map ReservedInstancesConfiguration.to_json v)
let of_json j = Aws.Json.to_list ReservedInstancesConfiguration.of_json j
