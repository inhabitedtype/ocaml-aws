open Aws.BaseTypes

type t = { watermark_key : String.t option }

let make ?watermark_key () = { watermark_key }

let parse xml =
  Some
    { watermark_key =
        Aws.Util.option_bind (Aws.Xml.member "watermarkKey" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.watermark_key (fun f ->
             Aws.Query.Pair ("WatermarkKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.watermark_key (fun f -> "watermarkKey", String.to_json f) ])

let of_json j =
  { watermark_key = Aws.Util.option_map (Aws.Json.lookup j "watermarkKey") String.of_json
  }
