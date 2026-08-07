type t = UnsuccessfulInstanceCreditSpecificationItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       UnsuccessfulInstanceCreditSpecificationItem.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list UnsuccessfulInstanceCreditSpecificationItem.to_query v

let to_json v = `List (List.map UnsuccessfulInstanceCreditSpecificationItem.to_json v)
let of_json j = Aws.Json.to_list UnsuccessfulInstanceCreditSpecificationItem.of_json j
