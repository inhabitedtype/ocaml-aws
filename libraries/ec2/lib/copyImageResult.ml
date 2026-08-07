open Aws.BaseTypes

type t = { image_id : String.t option }

let make ?image_id () = { image_id }

let parse xml =
  Some { image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f) ])

let of_json j =
  { image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json }
