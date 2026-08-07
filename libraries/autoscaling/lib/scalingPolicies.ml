type t = ScalingPolicy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ScalingPolicy.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ScalingPolicy.to_query v
let to_json v = `List (List.map ScalingPolicy.to_json v)
let of_json j = Aws.Json.to_list ScalingPolicy.of_json j
