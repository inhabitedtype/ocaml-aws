type t = InstanceEventWindowTimeRangeRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceEventWindowTimeRangeRequest.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InstanceEventWindowTimeRangeRequest.to_query v
let to_json v = `List (List.map InstanceEventWindowTimeRangeRequest.to_json v)
let of_json j = Aws.Json.to_list InstanceEventWindowTimeRangeRequest.of_json j
