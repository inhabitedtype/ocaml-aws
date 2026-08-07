type t = InstanceEventWindowTimeRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceEventWindowTimeRange.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceEventWindowTimeRange.to_query v
let to_json v = `List (List.map InstanceEventWindowTimeRange.to_json v)
let of_json j = Aws.Json.to_list InstanceEventWindowTimeRange.of_json j
