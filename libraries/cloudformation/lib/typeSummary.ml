open Aws.BaseTypes

type t =
  { type_ : RegistryType.t option
  ; type_name : String.t option
  ; default_version_id : String.t option
  ; type_arn : String.t option
  ; last_updated : DateTime.t option
  ; description : String.t option
  ; publisher_id : String.t option
  ; original_type_name : String.t option
  ; public_version_number : String.t option
  ; latest_public_version : String.t option
  ; publisher_identity : IdentityProvider.t option
  ; publisher_name : String.t option
  ; is_activated : Boolean.t option
  }

let make
    ?type_
    ?type_name
    ?default_version_id
    ?type_arn
    ?last_updated
    ?description
    ?publisher_id
    ?original_type_name
    ?public_version_number
    ?latest_public_version
    ?publisher_identity
    ?publisher_name
    ?is_activated
    () =
  { type_
  ; type_name
  ; default_version_id
  ; type_arn
  ; last_updated
  ; description
  ; publisher_id
  ; original_type_name
  ; public_version_number
  ; latest_public_version
  ; publisher_identity
  ; publisher_name
  ; is_activated
  }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; default_version_id =
        Aws.Util.option_bind (Aws.Xml.member "DefaultVersionId" xml) String.parse
    ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; last_updated =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdated" xml) DateTime.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    ; original_type_name =
        Aws.Util.option_bind (Aws.Xml.member "OriginalTypeName" xml) String.parse
    ; public_version_number =
        Aws.Util.option_bind (Aws.Xml.member "PublicVersionNumber" xml) String.parse
    ; latest_public_version =
        Aws.Util.option_bind (Aws.Xml.member "LatestPublicVersion" xml) String.parse
    ; publisher_identity =
        Aws.Util.option_bind
          (Aws.Xml.member "PublisherIdentity" xml)
          IdentityProvider.parse
    ; publisher_name =
        Aws.Util.option_bind (Aws.Xml.member "PublisherName" xml) String.parse
    ; is_activated = Aws.Util.option_bind (Aws.Xml.member "IsActivated" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_activated (fun f ->
             Aws.Query.Pair ("IsActivated", Boolean.to_query f))
       ; Aws.Util.option_map v.publisher_name (fun f ->
             Aws.Query.Pair ("PublisherName", String.to_query f))
       ; Aws.Util.option_map v.publisher_identity (fun f ->
             Aws.Query.Pair ("PublisherIdentity", IdentityProvider.to_query f))
       ; Aws.Util.option_map v.latest_public_version (fun f ->
             Aws.Query.Pair ("LatestPublicVersion", String.to_query f))
       ; Aws.Util.option_map v.public_version_number (fun f ->
             Aws.Query.Pair ("PublicVersionNumber", String.to_query f))
       ; Aws.Util.option_map v.original_type_name (fun f ->
             Aws.Query.Pair ("OriginalTypeName", String.to_query f))
       ; Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.last_updated (fun f ->
             Aws.Query.Pair ("LastUpdated", DateTime.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ; Aws.Util.option_map v.default_version_id (fun f ->
             Aws.Query.Pair ("DefaultVersionId", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_activated (fun f -> "IsActivated", Boolean.to_json f)
       ; Aws.Util.option_map v.publisher_name (fun f -> "PublisherName", String.to_json f)
       ; Aws.Util.option_map v.publisher_identity (fun f ->
             "PublisherIdentity", IdentityProvider.to_json f)
       ; Aws.Util.option_map v.latest_public_version (fun f ->
             "LatestPublicVersion", String.to_json f)
       ; Aws.Util.option_map v.public_version_number (fun f ->
             "PublicVersionNumber", String.to_json f)
       ; Aws.Util.option_map v.original_type_name (fun f ->
             "OriginalTypeName", String.to_json f)
       ; Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.last_updated (fun f -> "LastUpdated", DateTime.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ; Aws.Util.option_map v.default_version_id (fun f ->
             "DefaultVersionId", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; default_version_id =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultVersionId") String.of_json
  ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; last_updated = Aws.Util.option_map (Aws.Json.lookup j "LastUpdated") DateTime.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  ; original_type_name =
      Aws.Util.option_map (Aws.Json.lookup j "OriginalTypeName") String.of_json
  ; public_version_number =
      Aws.Util.option_map (Aws.Json.lookup j "PublicVersionNumber") String.of_json
  ; latest_public_version =
      Aws.Util.option_map (Aws.Json.lookup j "LatestPublicVersion") String.of_json
  ; publisher_identity =
      Aws.Util.option_map (Aws.Json.lookup j "PublisherIdentity") IdentityProvider.of_json
  ; publisher_name =
      Aws.Util.option_map (Aws.Json.lookup j "PublisherName") String.of_json
  ; is_activated = Aws.Util.option_map (Aws.Json.lookup j "IsActivated") Boolean.of_json
  }
