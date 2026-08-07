type t = ReservedInstancesModificationResult.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReservedInstancesModificationResult.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ReservedInstancesModificationResult.to_query v
let to_json v = `List (List.map ReservedInstancesModificationResult.to_json v)
let of_json j = Aws.Json.to_list ReservedInstancesModificationResult.of_json j
