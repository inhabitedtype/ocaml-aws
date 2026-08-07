type t = InstanceCreditSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceCreditSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceCreditSpecification.to_query v
let to_json v = `List (List.map InstanceCreditSpecification.to_json v)
let of_json j = Aws.Json.to_list InstanceCreditSpecification.of_json j
