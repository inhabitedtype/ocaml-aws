open Aws.BaseTypes

type t =
  { image_data : String.t option
  ; instance_id : String.t option
  }

let make ?image_data ?instance_id () = { image_data; instance_id }

let parse xml =
  Some
    { image_data = Aws.Util.option_bind (Aws.Xml.member "imageData" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.image_data (fun f ->
             Aws.Query.Pair ("ImageData", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.image_data (fun f -> "imageData", String.to_json f)
       ])

let of_json j =
  { image_data = Aws.Util.option_map (Aws.Json.lookup j "imageData") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  }
