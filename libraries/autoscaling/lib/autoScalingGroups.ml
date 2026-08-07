type t = AutoScalingGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AutoScalingGroup.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AutoScalingGroup.to_query v
let to_json v = `List (List.map AutoScalingGroup.to_json v)
let of_json j = Aws.Json.to_list AutoScalingGroup.of_json j
