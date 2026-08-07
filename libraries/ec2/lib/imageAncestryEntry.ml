open Aws.BaseTypes

type t =
  { creation_date : DateTime.t option
  ; image_id : String.t option
  ; image_owner_alias : String.t option
  ; source_image_id : String.t option
  ; source_image_region : String.t option
  }

let make
    ?creation_date
    ?image_id
    ?image_owner_alias
    ?source_image_id
    ?source_image_region
    () =
  { creation_date; image_id; image_owner_alias; source_image_id; source_image_region }

let parse xml =
  Some
    { creation_date =
        Aws.Util.option_bind (Aws.Xml.member "creationDate" xml) DateTime.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; image_owner_alias =
        Aws.Util.option_bind (Aws.Xml.member "imageOwnerAlias" xml) String.parse
    ; source_image_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageId" xml) String.parse
    ; source_image_region =
        Aws.Util.option_bind (Aws.Xml.member "sourceImageRegion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_image_region (fun f ->
             Aws.Query.Pair ("SourceImageRegion", String.to_query f))
       ; Aws.Util.option_map v.source_image_id (fun f ->
             Aws.Query.Pair ("SourceImageId", String.to_query f))
       ; Aws.Util.option_map v.image_owner_alias (fun f ->
             Aws.Query.Pair ("ImageOwnerAlias", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_image_region (fun f ->
             "sourceImageRegion", String.to_json f)
       ; Aws.Util.option_map v.source_image_id (fun f ->
             "sourceImageId", String.to_json f)
       ; Aws.Util.option_map v.image_owner_alias (fun f ->
             "imageOwnerAlias", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "creationDate", DateTime.to_json f)
       ])

let of_json j =
  { creation_date =
      Aws.Util.option_map (Aws.Json.lookup j "creationDate") DateTime.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; image_owner_alias =
      Aws.Util.option_map (Aws.Json.lookup j "imageOwnerAlias") String.of_json
  ; source_image_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageId") String.of_json
  ; source_image_region =
      Aws.Util.option_map (Aws.Json.lookup j "sourceImageRegion") String.of_json
  }
