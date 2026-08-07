open Aws.BaseTypes

type t =
  { global_cluster_identifier : String.t
  ; new_global_cluster_identifier : String.t option
  ; deletion_protection : Boolean.t option
  ; engine_version : String.t option
  ; allow_major_version_upgrade : Boolean.t option
  }

let make
    ~global_cluster_identifier
    ?new_global_cluster_identifier
    ?deletion_protection
    ?engine_version
    ?allow_major_version_upgrade
    () =
  { global_cluster_identifier
  ; new_global_cluster_identifier
  ; deletion_protection
  ; engine_version
  ; allow_major_version_upgrade
  }

let parse xml =
  Some
    { global_cluster_identifier =
        Aws.Xml.required
          "GlobalClusterIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalClusterIdentifier" xml)
             String.parse)
    ; new_global_cluster_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "NewGlobalClusterIdentifier" xml)
          String.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; allow_major_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AllowMajorVersionUpgrade" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allow_major_version_upgrade (fun f ->
             Aws.Query.Pair ("AllowMajorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.new_global_cluster_identifier (fun f ->
             Aws.Query.Pair ("NewGlobalClusterIdentifier", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("GlobalClusterIdentifier", String.to_query v.global_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allow_major_version_upgrade (fun f ->
             "AllowMajorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.new_global_cluster_identifier (fun f ->
             "NewGlobalClusterIdentifier", String.to_json f)
       ; Some ("GlobalClusterIdentifier", String.to_json v.global_cluster_identifier)
       ])

let of_json j =
  { global_cluster_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalClusterIdentifier"))
  ; new_global_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "NewGlobalClusterIdentifier") String.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; allow_major_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AllowMajorVersionUpgrade") Boolean.of_json
  }
