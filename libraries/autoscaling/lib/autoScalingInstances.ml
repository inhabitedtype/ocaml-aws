type t = AutoScalingInstanceDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AutoScalingInstanceDetails.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AutoScalingInstanceDetails.to_query v
let to_json v = `List (List.map AutoScalingInstanceDetails.to_json v)
let of_json j = Aws.Json.to_list AutoScalingInstanceDetails.of_json j
