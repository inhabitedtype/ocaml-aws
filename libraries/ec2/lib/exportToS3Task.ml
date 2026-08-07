open Aws.BaseTypes

type t =
  { container_format : ContainerFormat.t option
  ; disk_image_format : DiskImageFormat.t option
  ; s3_bucket : String.t option
  ; s3_key : String.t option
  }

let make ?container_format ?disk_image_format ?s3_bucket ?s3_key () =
  { container_format; disk_image_format; s3_bucket; s3_key }

let parse xml =
  Some
    { container_format =
        Aws.Util.option_bind (Aws.Xml.member "containerFormat" xml) ContainerFormat.parse
    ; disk_image_format =
        Aws.Util.option_bind (Aws.Xml.member "diskImageFormat" xml) DiskImageFormat.parse
    ; s3_bucket = Aws.Util.option_bind (Aws.Xml.member "s3Bucket" xml) String.parse
    ; s3_key = Aws.Util.option_bind (Aws.Xml.member "s3Key" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_key (fun f ->
             Aws.Query.Pair ("S3Key", String.to_query f))
       ; Aws.Util.option_map v.s3_bucket (fun f ->
             Aws.Query.Pair ("S3Bucket", String.to_query f))
       ; Aws.Util.option_map v.disk_image_format (fun f ->
             Aws.Query.Pair ("DiskImageFormat", DiskImageFormat.to_query f))
       ; Aws.Util.option_map v.container_format (fun f ->
             Aws.Query.Pair ("ContainerFormat", ContainerFormat.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_key (fun f -> "s3Key", String.to_json f)
       ; Aws.Util.option_map v.s3_bucket (fun f -> "s3Bucket", String.to_json f)
       ; Aws.Util.option_map v.disk_image_format (fun f ->
             "diskImageFormat", DiskImageFormat.to_json f)
       ; Aws.Util.option_map v.container_format (fun f ->
             "containerFormat", ContainerFormat.to_json f)
       ])

let of_json j =
  { container_format =
      Aws.Util.option_map (Aws.Json.lookup j "containerFormat") ContainerFormat.of_json
  ; disk_image_format =
      Aws.Util.option_map (Aws.Json.lookup j "diskImageFormat") DiskImageFormat.of_json
  ; s3_bucket = Aws.Util.option_map (Aws.Json.lookup j "s3Bucket") String.of_json
  ; s3_key = Aws.Util.option_map (Aws.Json.lookup j "s3Key") String.of_json
  }
