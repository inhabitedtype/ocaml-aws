open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; source_fpga_image_id : String.t
  ; description : String.t option
  ; name : String.t option
  ; source_region : String.t
  ; client_token : String.t option
  }

let make ?dry_run ~source_fpga_image_id ?description ?name ~source_region ?client_token ()
    =
  { dry_run; source_fpga_image_id; description; name; source_region; client_token }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; source_fpga_image_id =
        Aws.Xml.required
          "SourceFpgaImageId"
          (Aws.Util.option_bind (Aws.Xml.member "SourceFpgaImageId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; source_region =
        Aws.Xml.required
          "SourceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "SourceRegion" xml) String.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("SourceRegion", String.to_query v.source_region))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair ("SourceFpgaImageId", String.to_query v.source_fpga_image_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("SourceRegion", String.to_json v.source_region)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("SourceFpgaImageId", String.to_json v.source_fpga_image_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; source_fpga_image_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceFpgaImageId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; source_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceRegion"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
