type t = InstanceCreditSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceCreditSpecificationRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceCreditSpecificationRequest.to_query v
let to_json v = `List (List.map InstanceCreditSpecificationRequest.to_json v)
let of_json j = Aws.Json.to_list InstanceCreditSpecificationRequest.of_json j
