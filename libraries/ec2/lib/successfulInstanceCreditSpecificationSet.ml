type t = SuccessfulInstanceCreditSpecificationItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       SuccessfulInstanceCreditSpecificationItem.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list SuccessfulInstanceCreditSpecificationItem.to_query v

let to_json v = `List (List.map SuccessfulInstanceCreditSpecificationItem.to_json v)
let of_json j = Aws.Json.to_list SuccessfulInstanceCreditSpecificationItem.of_json j
