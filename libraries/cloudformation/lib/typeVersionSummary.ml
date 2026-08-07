open Aws.BaseTypes

type t =
  { type_ : RegistryType.t option
  ; type_name : String.t option
  ; version_id : String.t option
  ; is_default_version : Boolean.t option
  ; arn : String.t option
  ; time_created : DateTime.t option
  ; description : String.t option
  ; public_version_number : String.t option
  }

let make
    ?type_
    ?type_name
    ?version_id
    ?is_default_version
    ?arn
    ?time_created
    ?description
    ?public_version_number
    () =
  { type_
  ; type_name
  ; version_id
  ; is_default_version
  ; arn
  ; time_created
  ; description
  ; public_version_number
  }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; version_id = Aws.Util.option_bind (Aws.Xml.member "VersionId" xml) String.parse
    ; is_default_version =
        Aws.Util.option_bind (Aws.Xml.member "IsDefaultVersion" xml) Boolean.parse
    ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; time_created =
        Aws.Util.option_bind (Aws.Xml.member "TimeCreated" xml) DateTime.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; public_version_number =
        Aws.Util.option_bind (Aws.Xml.member "PublicVersionNumber" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_version_number (fun f ->
             Aws.Query.Pair ("PublicVersionNumber", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.time_created (fun f ->
             Aws.Query.Pair ("TimeCreated", DateTime.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ; Aws.Util.option_map v.is_default_version (fun f ->
             Aws.Query.Pair ("IsDefaultVersion", Boolean.to_query f))
       ; Aws.Util.option_map v.version_id (fun f ->
             Aws.Query.Pair ("VersionId", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_version_number (fun f ->
             "PublicVersionNumber", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.time_created (fun f -> "TimeCreated", DateTime.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ; Aws.Util.option_map v.is_default_version (fun f ->
             "IsDefaultVersion", Boolean.to_json f)
       ; Aws.Util.option_map v.version_id (fun f -> "VersionId", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; version_id = Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json
  ; is_default_version =
      Aws.Util.option_map (Aws.Json.lookup j "IsDefaultVersion") Boolean.of_json
  ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; time_created = Aws.Util.option_map (Aws.Json.lookup j "TimeCreated") DateTime.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; public_version_number =
      Aws.Util.option_map (Aws.Json.lookup j "PublicVersionNumber") String.of_json
  }
