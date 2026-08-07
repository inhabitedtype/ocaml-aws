open Aws.BaseTypes

type t =
  { engine : String.t
  ; engine_version : String.t
  ; database_installation_files_s3_bucket_name : String.t option
  ; database_installation_files_s3_prefix : String.t option
  ; database_installation_files : StringList.t
  ; image_id : String.t option
  ; k_m_s_key_id : String.t option
  ; source_custom_db_engine_version_identifier : String.t option
  ; use_aws_provided_latest_image : Boolean.t option
  ; description : String.t option
  ; manifest : String.t option
  ; tags : TagList.t
  }

let make
    ~engine
    ~engine_version
    ?database_installation_files_s3_bucket_name
    ?database_installation_files_s3_prefix
    ?(database_installation_files = [])
    ?image_id
    ?k_m_s_key_id
    ?source_custom_db_engine_version_identifier
    ?use_aws_provided_latest_image
    ?description
    ?manifest
    ?(tags = [])
    () =
  { engine
  ; engine_version
  ; database_installation_files_s3_bucket_name
  ; database_installation_files_s3_prefix
  ; database_installation_files
  ; image_id
  ; k_m_s_key_id
  ; source_custom_db_engine_version_identifier
  ; use_aws_provided_latest_image
  ; description
  ; manifest
  ; tags
  }

let parse xml =
  Some
    { engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; engine_version =
        Aws.Xml.required
          "EngineVersion"
          (Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse)
    ; database_installation_files_s3_bucket_name =
        Aws.Util.option_bind
          (Aws.Xml.member "DatabaseInstallationFilesS3BucketName" xml)
          String.parse
    ; database_installation_files_s3_prefix =
        Aws.Util.option_bind
          (Aws.Xml.member "DatabaseInstallationFilesS3Prefix" xml)
          String.parse
    ; database_installation_files =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DatabaseInstallationFiles" xml)
             StringList.parse)
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    ; k_m_s_key_id = Aws.Util.option_bind (Aws.Xml.member "KMSKeyId" xml) String.parse
    ; source_custom_db_engine_version_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceCustomDbEngineVersionIdentifier" xml)
          String.parse
    ; use_aws_provided_latest_image =
        Aws.Util.option_bind
          (Aws.Xml.member "UseAwsProvidedLatestImage" xml)
          Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; manifest = Aws.Util.option_bind (Aws.Xml.member "Manifest" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.manifest (fun f ->
             Aws.Query.Pair ("Manifest", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.use_aws_provided_latest_image (fun f ->
             Aws.Query.Pair ("UseAwsProvidedLatestImage", Boolean.to_query f))
       ; Aws.Util.option_map v.source_custom_db_engine_version_identifier (fun f ->
             Aws.Query.Pair ("SourceCustomDbEngineVersionIdentifier", String.to_query f))
       ; Aws.Util.option_map v.k_m_s_key_id (fun f ->
             Aws.Query.Pair ("KMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DatabaseInstallationFiles.member"
              , StringList.to_query v.database_installation_files ))
       ; Aws.Util.option_map v.database_installation_files_s3_prefix (fun f ->
             Aws.Query.Pair ("DatabaseInstallationFilesS3Prefix", String.to_query f))
       ; Aws.Util.option_map v.database_installation_files_s3_bucket_name (fun f ->
             Aws.Query.Pair ("DatabaseInstallationFilesS3BucketName", String.to_query f))
       ; Some (Aws.Query.Pair ("EngineVersion", String.to_query v.engine_version))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.manifest (fun f -> "Manifest", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.use_aws_provided_latest_image (fun f ->
             "UseAwsProvidedLatestImage", Boolean.to_json f)
       ; Aws.Util.option_map v.source_custom_db_engine_version_identifier (fun f ->
             "SourceCustomDbEngineVersionIdentifier", String.to_json f)
       ; Aws.Util.option_map v.k_m_s_key_id (fun f -> "KMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Some
           ("DatabaseInstallationFiles", StringList.to_json v.database_installation_files)
       ; Aws.Util.option_map v.database_installation_files_s3_prefix (fun f ->
             "DatabaseInstallationFilesS3Prefix", String.to_json f)
       ; Aws.Util.option_map v.database_installation_files_s3_bucket_name (fun f ->
             "DatabaseInstallationFilesS3BucketName", String.to_json f)
       ; Some ("EngineVersion", String.to_json v.engine_version)
       ; Some ("Engine", String.to_json v.engine)
       ])

let of_json j =
  { engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; engine_version =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineVersion"))
  ; database_installation_files_s3_bucket_name =
      Aws.Util.option_map
        (Aws.Json.lookup j "DatabaseInstallationFilesS3BucketName")
        String.of_json
  ; database_installation_files_s3_prefix =
      Aws.Util.option_map
        (Aws.Json.lookup j "DatabaseInstallationFilesS3Prefix")
        String.of_json
  ; database_installation_files =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DatabaseInstallationFiles"))
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  ; k_m_s_key_id = Aws.Util.option_map (Aws.Json.lookup j "KMSKeyId") String.of_json
  ; source_custom_db_engine_version_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceCustomDbEngineVersionIdentifier")
        String.of_json
  ; use_aws_provided_latest_image =
      Aws.Util.option_map (Aws.Json.lookup j "UseAwsProvidedLatestImage") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; manifest = Aws.Util.option_map (Aws.Json.lookup j "Manifest") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
