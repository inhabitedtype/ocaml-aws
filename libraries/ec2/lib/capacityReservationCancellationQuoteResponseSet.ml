type t = CapacityReservationCancellationQuote.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityReservationCancellationQuote.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityReservationCancellationQuote.to_query v
let to_json v = `List (List.map CapacityReservationCancellationQuote.to_json v)
let of_json j = Aws.Json.to_list CapacityReservationCancellationQuote.of_json j
