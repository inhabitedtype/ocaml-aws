open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; description : String.t option
  ; disk_image_format : DiskImageFormat.t
  ; dry_run : Boolean.t option
  ; image_id : String.t
  ; s3_export_location : ExportTaskS3LocationRequest.t
  ; role_name : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?client_token
    ?description
    ~disk_image_format
    ?dry_run
    ~image_id
    ~s3_export_location
    ?role_name
    ?(tag_specifications = [])
    () =
  { client_token
  ; description
  ; disk_image_format
  ; dry_run
  ; image_id
  ; s3_export_location
  ; role_name
  ; tag_specifications
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; disk_image_format =
        Aws.Xml.required
          "DiskImageFormat"
          (Aws.Util.option_bind
             (Aws.Xml.member "DiskImageFormat" xml)
             DiskImageFormat.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; s3_export_location =
        Aws.Xml.required
          "S3ExportLocation"
          (Aws.Util.option_bind
             (Aws.Xml.member "S3ExportLocation" xml)
             ExportTaskS3LocationRequest.parse)
    ; role_name = Aws.Util.option_bind (Aws.Xml.member "RoleName" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.role_name (fun f ->
             Aws.Query.Pair ("RoleName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "S3ExportLocation"
              , ExportTaskS3LocationRequest.to_query v.s3_export_location ))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DiskImageFormat", DiskImageFormat.to_query v.disk_image_format))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.role_name (fun f -> "RoleName", String.to_json f)
       ; Some
           ("S3ExportLocation", ExportTaskS3LocationRequest.to_json v.s3_export_location)
       ; Some ("ImageId", String.to_json v.image_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("DiskImageFormat", DiskImageFormat.to_json v.disk_image_format)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; disk_image_format =
      DiskImageFormat.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DiskImageFormat"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; s3_export_location =
      ExportTaskS3LocationRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "S3ExportLocation"))
  ; role_name = Aws.Util.option_map (Aws.Json.lookup j "RoleName") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
