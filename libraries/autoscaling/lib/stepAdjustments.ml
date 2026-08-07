type t = StepAdjustment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StepAdjustment.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StepAdjustment.to_query v
let to_json v = `List (List.map StepAdjustment.to_json v)
let of_json j = Aws.Json.to_list StepAdjustment.of_json j
