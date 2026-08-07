open Aws.BaseTypes

type t =
  { image_id : String.t option
  ; name : String.t option
  ; owner_id : String.t option
  ; state : ImageState.t option
  ; image_owner_alias : String.t option
  ; creation_date : String.t option
  ; deprecation_time : String.t option
  ; image_allowed : Boolean.t option
  ; is_public : Boolean.t option
  ; image_watermarks : ImageWatermarkList.t
  }

let make
    ?image_id
    ?name
    ?owner_id
    ?state
    ?image_owner_alias
    ?creation_date
    ?deprecation_time
    ?image_allowed
    ?is_public
    ?(image_watermarks = [])
    () =
  { image_id
  ; name
  ; owner_id
  ; state
  ; image_owner_alias
  ; creation_date
  ; deprecation_time
  ; image_allowed
  ; is_public
  ; image_watermarks
  }

let parse xml =
  Some
    { image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "imageOwnerId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "imageState" xml) ImageState.parse
    ; image_owner_alias =
        Aws.Util.option_bind (Aws.Xml.member "imageOwnerAlias" xml) String.parse
    ; creation_date =
        Aws.Util.option_bind (Aws.Xml.member "creationDate" xml) String.parse
    ; deprecation_time =
        Aws.Util.option_bind (Aws.Xml.member "deprecationTime" xml) String.parse
    ; image_allowed =
        Aws.Util.option_bind (Aws.Xml.member "imageAllowed" xml) Boolean.parse
    ; is_public = Aws.Util.option_bind (Aws.Xml.member "isPublic" xml) Boolean.parse
    ; image_watermarks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageWatermarkSet" xml)
             ImageWatermarkList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ImageWatermarkSet", ImageWatermarkList.to_query v.image_watermarks))
       ; Aws.Util.option_map v.is_public (fun f ->
             Aws.Query.Pair ("IsPublic", Boolean.to_query f))
       ; Aws.Util.option_map v.image_allowed (fun f ->
             Aws.Query.Pair ("ImageAllowed", Boolean.to_query f))
       ; Aws.Util.option_map v.deprecation_time (fun f ->
             Aws.Query.Pair ("DeprecationTime", String.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", String.to_query f))
       ; Aws.Util.option_map v.image_owner_alias (fun f ->
             Aws.Query.Pair ("ImageOwnerAlias", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("ImageState", ImageState.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("ImageOwnerId", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("imageWatermarkSet", ImageWatermarkList.to_json v.image_watermarks)
       ; Aws.Util.option_map v.is_public (fun f -> "isPublic", Boolean.to_json f)
       ; Aws.Util.option_map v.image_allowed (fun f -> "imageAllowed", Boolean.to_json f)
       ; Aws.Util.option_map v.deprecation_time (fun f ->
             "deprecationTime", String.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "creationDate", String.to_json f)
       ; Aws.Util.option_map v.image_owner_alias (fun f ->
             "imageOwnerAlias", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "imageState", ImageState.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "imageOwnerId", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ])

let of_json j =
  { image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "imageOwnerId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "imageState") ImageState.of_json
  ; image_owner_alias =
      Aws.Util.option_map (Aws.Json.lookup j "imageOwnerAlias") String.of_json
  ; creation_date = Aws.Util.option_map (Aws.Json.lookup j "creationDate") String.of_json
  ; deprecation_time =
      Aws.Util.option_map (Aws.Json.lookup j "deprecationTime") String.of_json
  ; image_allowed = Aws.Util.option_map (Aws.Json.lookup j "imageAllowed") Boolean.of_json
  ; is_public = Aws.Util.option_map (Aws.Json.lookup j "isPublic") Boolean.of_json
  ; image_watermarks =
      ImageWatermarkList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageWatermarkSet"))
  }
