type t = ReservedInstancesListing.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReservedInstancesListing.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ReservedInstancesListing.to_query v
let to_json v = `List (List.map ReservedInstancesListing.to_json v)
let of_json j = Aws.Json.to_list ReservedInstancesListing.of_json j
