type t = ReshardingConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ReshardingConfiguration.parse
       (Aws.Xml.members "ReshardingConfiguration" xml))

let to_query v = Aws.Query.to_query_list ReshardingConfiguration.to_query v
let to_json v = `List (List.map ReshardingConfiguration.to_json v)
let of_json j = Aws.Json.to_list ReshardingConfiguration.of_json j
