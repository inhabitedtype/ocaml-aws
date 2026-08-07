open Aws.BaseTypes

type t =
  { image_id : String.t option
  ; resource_type : ImageReferenceResourceType.t option
  ; arn : String.t option
  }

let make ?image_id ?resource_type ?arn () = { image_id; resource_type; arn }

let parse xml =
  Some
    { image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          ImageReferenceResourceType.parse
    ; arn = Aws.Util.option_bind (Aws.Xml.member "arn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", ImageReferenceResourceType.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> "arn", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", ImageReferenceResourceType.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ])

let of_json j =
  { image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        ImageReferenceResourceType.of_json
  ; arn = Aws.Util.option_map (Aws.Json.lookup j "arn") String.of_json
  }
