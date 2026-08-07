type t = CancelSpotFleetRequestsSuccessItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CancelSpotFleetRequestsSuccessItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CancelSpotFleetRequestsSuccessItem.to_query v
let to_json v = `List (List.map CancelSpotFleetRequestsSuccessItem.to_json v)
let of_json j = Aws.Json.to_list CancelSpotFleetRequestsSuccessItem.of_json j
