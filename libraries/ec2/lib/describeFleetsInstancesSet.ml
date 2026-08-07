type t = DescribeFleetsInstances.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DescribeFleetsInstances.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DescribeFleetsInstances.to_query v
let to_json v = `List (List.map DescribeFleetsInstances.to_json v)
let of_json j = Aws.Json.to_list DescribeFleetsInstances.of_json j
