open Aws.BaseTypes

type t =
  { watermark_key : String.t option
  ; source_image_region : String.t option
  ; source_image_id : String.t option
  ; source_image_creation_time : DateTime.t option
  ; watermark_creation_time : DateTime.t option
  }

let make
    ?watermark_key
    ?source_image_region
    ?source_image_id
    ?source_image_creation_time
    ?watermark_creation_time
    () =
  { watermark_key
  ; source_image_region
  ; source_image_id
  ; source_image_creation_time
  ; watermark_creation_time
  }

let parse xml =
  Some
    { watermark_key =
        Aws.Util.option_bind (Aws.Xml.member "watermarkKey" xml) String.parse
    ; source_image_region =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageRegion" xml) String.parse
    ; source_image_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageId" xml) String.parse
    ; source_image_creation_time =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageCreationTime" xml) DateTime.parse
    ; watermark_creation_time =
        Aws.Util.option_bind (Aws.Xml.member "watermarkCreationTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.watermark_creation_time (fun f ->
             Aws.Query.Pair ("WatermarkCreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.source_image_creation_time (fun f ->
             Aws.Query.Pair ("SourceImageCreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.source_image_id (fun f ->
             Aws.Query.Pair ("SourceImageId", String.to_query f))
       ; Aws.Util.option_map v.source_image_region (fun f ->
             Aws.Query.Pair ("SourceImageRegion", String.to_query f))
       ; Aws.Util.option_map v.watermark_key (fun f ->
             Aws.Query.Pair ("WatermarkKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.watermark_creation_time (fun f ->
             "watermarkCreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.source_image_creation_time (fun f ->
             "sourceImageCreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.source_image_id (fun f ->
             "sourceImageId", String.to_json f)
       ; Aws.Util.option_map v.source_image_region (fun f ->
             "sourceImageRegion", String.to_json f)
       ; Aws.Util.option_map v.watermark_key (fun f -> "watermarkKey", String.to_json f)
       ])

let of_json j =
  { watermark_key = Aws.Util.option_map (Aws.Json.lookup j "watermarkKey") String.of_json
  ; source_image_region =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageRegion") String.of_json
  ; source_image_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageId") String.of_json
  ; source_image_creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageCreationTime") DateTime.of_json
  ; watermark_creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "watermarkCreationTime") DateTime.of_json
  }
