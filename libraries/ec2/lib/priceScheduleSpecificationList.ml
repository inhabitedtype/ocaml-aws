type t = PriceScheduleSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PriceScheduleSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PriceScheduleSpecification.to_query v
let to_json v = `List (List.map PriceScheduleSpecification.to_json v)
let of_json j = Aws.Json.to_list PriceScheduleSpecification.of_json j
