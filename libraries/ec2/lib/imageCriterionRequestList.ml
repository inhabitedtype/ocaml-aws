type t = ImageCriterionRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImageCriterionRequest.parse (Aws.Xml.members "ImageCriterion" xml))

let to_query v = Aws.Query.to_query_list ImageCriterionRequest.to_query v
let to_json v = `List (List.map ImageCriterionRequest.to_json v)
let of_json j = Aws.Json.to_list ImageCriterionRequest.of_json j
