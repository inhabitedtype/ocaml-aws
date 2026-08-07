open Aws.BaseTypes

type t =
  { type_name_alias : String.t option
  ; original_type_name : String.t option
  ; publisher_id : String.t option
  ; supported_major_versions : SupportedMajorVersions.t
  }

let make
    ?type_name_alias
    ?original_type_name
    ?publisher_id
    ?(supported_major_versions = [])
    () =
  { type_name_alias; original_type_name; publisher_id; supported_major_versions }

let parse xml =
  Some
    { type_name_alias =
        Aws.Util.option_bind (Aws.Xml.member "TypeNameAlias" xml) String.parse
    ; original_type_name =
        Aws.Util.option_bind (Aws.Xml.member "OriginalTypeName" xml) String.parse
    ; publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    ; supported_major_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedMajorVersions" xml)
             SupportedMajorVersions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedMajorVersions.member"
              , SupportedMajorVersions.to_query v.supported_major_versions ))
       ; Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ; Aws.Util.option_map v.original_type_name (fun f ->
             Aws.Query.Pair ("OriginalTypeName", String.to_query f))
       ; Aws.Util.option_map v.type_name_alias (fun f ->
             Aws.Query.Pair ("TypeNameAlias", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "SupportedMajorVersions"
           , SupportedMajorVersions.to_json v.supported_major_versions )
       ; Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ; Aws.Util.option_map v.original_type_name (fun f ->
             "OriginalTypeName", String.to_json f)
       ; Aws.Util.option_map v.type_name_alias (fun f ->
             "TypeNameAlias", String.to_json f)
       ])

let of_json j =
  { type_name_alias =
      Aws.Util.option_map (Aws.Json.lookup j "TypeNameAlias") String.of_json
  ; original_type_name =
      Aws.Util.option_map (Aws.Json.lookup j "OriginalTypeName") String.of_json
  ; publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  ; supported_major_versions =
      SupportedMajorVersions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedMajorVersions"))
  }
