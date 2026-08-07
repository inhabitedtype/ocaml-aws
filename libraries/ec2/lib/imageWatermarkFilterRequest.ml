open Aws.BaseTypes

type t =
  { watermark_key : String.t option
  ; source_image_region : String.t option
  ; maximum_days_since_source_image_created : Integer.t option
  ; maximum_days_since_watermark_created : Integer.t option
  }

let make
    ?watermark_key
    ?source_image_region
    ?maximum_days_since_source_image_created
    ?maximum_days_since_watermark_created
    () =
  { watermark_key
  ; source_image_region
  ; maximum_days_since_source_image_created
  ; maximum_days_since_watermark_created
  }

let parse xml =
  Some
    { watermark_key =
        Aws.Util.option_bind (Aws.Xml.member "WatermarkKey" xml) String.parse
    ; source_image_region =
        Aws.Util.option_bind (Aws.Xml.member "SourceImageRegion" xml) String.parse
    ; maximum_days_since_source_image_created =
        Aws.Util.option_bind
          (Aws.Xml.member "MaximumDaysSinceSourceImageCreated" xml)
          Integer.parse
    ; maximum_days_since_watermark_created =
        Aws.Util.option_bind
          (Aws.Xml.member "MaximumDaysSinceWatermarkCreated" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_days_since_watermark_created (fun f ->
             Aws.Query.Pair ("MaximumDaysSinceWatermarkCreated", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_days_since_source_image_created (fun f ->
             Aws.Query.Pair ("MaximumDaysSinceSourceImageCreated", Integer.to_query f))
       ; Aws.Util.option_map v.source_image_region (fun f ->
             Aws.Query.Pair ("SourceImageRegion", String.to_query f))
       ; Aws.Util.option_map v.watermark_key (fun f ->
             Aws.Query.Pair ("WatermarkKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_days_since_watermark_created (fun f ->
             "MaximumDaysSinceWatermarkCreated", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_days_since_source_image_created (fun f ->
             "MaximumDaysSinceSourceImageCreated", Integer.to_json f)
       ; Aws.Util.option_map v.source_image_region (fun f ->
             "SourceImageRegion", String.to_json f)
       ; Aws.Util.option_map v.watermark_key (fun f -> "WatermarkKey", String.to_json f)
       ])

let of_json j =
  { watermark_key = Aws.Util.option_map (Aws.Json.lookup j "WatermarkKey") String.of_json
  ; source_image_region =
      Aws.Util.option_map (Aws.Json.lookup j "SourceImageRegion") String.of_json
  ; maximum_days_since_source_image_created =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaximumDaysSinceSourceImageCreated")
        Integer.of_json
  ; maximum_days_since_watermark_created =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaximumDaysSinceWatermarkCreated")
        Integer.of_json
  }
