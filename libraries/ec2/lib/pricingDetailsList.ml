type t = PricingDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PricingDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PricingDetail.to_query v
let to_json v = `List (List.map PricingDetail.to_json v)
let of_json j = Aws.Json.to_list PricingDetail.of_json j
