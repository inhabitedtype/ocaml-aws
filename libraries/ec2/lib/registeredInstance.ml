open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; sql_server_license_usage : SqlServerLicenseUsage.t option
  ; ha_status : HaStatus.t option
  ; processing_status : String.t option
  ; last_updated_time : DateTime.t option
  ; sql_server_credentials : String.t option
  ; tags : TagList.t
  }

let make
    ?instance_id
    ?sql_server_license_usage
    ?ha_status
    ?processing_status
    ?last_updated_time
    ?sql_server_credentials
    ?(tags = [])
    () =
  { instance_id
  ; sql_server_license_usage
  ; ha_status
  ; processing_status
  ; last_updated_time
  ; sql_server_credentials
  ; tags
  }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; sql_server_license_usage =
        Aws.Util.option_bind
          (Aws.Xml.member "sqlServerLicenseUsage" xml)
          SqlServerLicenseUsage.parse
    ; ha_status = Aws.Util.option_bind (Aws.Xml.member "haStatus" xml) HaStatus.parse
    ; processing_status =
        Aws.Util.option_bind (Aws.Xml.member "processingStatus" xml) String.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdatedTime" xml) DateTime.parse
    ; sql_server_credentials =
        Aws.Util.option_bind (Aws.Xml.member "sqlServerCredentials" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.sql_server_credentials (fun f ->
             Aws.Query.Pair ("SqlServerCredentials", String.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.processing_status (fun f ->
             Aws.Query.Pair ("ProcessingStatus", String.to_query f))
       ; Aws.Util.option_map v.ha_status (fun f ->
             Aws.Query.Pair ("HaStatus", HaStatus.to_query f))
       ; Aws.Util.option_map v.sql_server_license_usage (fun f ->
             Aws.Query.Pair ("SqlServerLicenseUsage", SqlServerLicenseUsage.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.sql_server_credentials (fun f ->
             "sqlServerCredentials", String.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "lastUpdatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.processing_status (fun f ->
             "processingStatus", String.to_json f)
       ; Aws.Util.option_map v.ha_status (fun f -> "haStatus", HaStatus.to_json f)
       ; Aws.Util.option_map v.sql_server_license_usage (fun f ->
             "sqlServerLicenseUsage", SqlServerLicenseUsage.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; sql_server_license_usage =
      Aws.Util.option_map
        (Aws.Json.lookup j "sqlServerLicenseUsage")
        SqlServerLicenseUsage.of_json
  ; ha_status = Aws.Util.option_map (Aws.Json.lookup j "haStatus") HaStatus.of_json
  ; processing_status =
      Aws.Util.option_map (Aws.Json.lookup j "processingStatus") String.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdatedTime") DateTime.of_json
  ; sql_server_credentials =
      Aws.Util.option_map (Aws.Json.lookup j "sqlServerCredentials") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
