open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_prefix_list_resolver_target_id : String.t
  ; desired_version : Long.t option
  ; track_latest_version : Boolean.t option
  ; client_token : String.t option
  }

let make
    ?dry_run
    ~ipam_prefix_list_resolver_target_id
    ?desired_version
    ?track_latest_version
    ?client_token
    () =
  { dry_run
  ; ipam_prefix_list_resolver_target_id
  ; desired_version
  ; track_latest_version
  ; client_token
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_prefix_list_resolver_target_id =
        Aws.Xml.required
          "IpamPrefixListResolverTargetId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamPrefixListResolverTargetId" xml)
             String.parse)
    ; desired_version =
        Aws.Util.option_bind (Aws.Xml.member "DesiredVersion" xml) Long.parse
    ; track_latest_version =
        Aws.Util.option_bind (Aws.Xml.member "TrackLatestVersion" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.track_latest_version (fun f ->
             Aws.Query.Pair ("TrackLatestVersion", Boolean.to_query f))
       ; Aws.Util.option_map v.desired_version (fun f ->
             Aws.Query.Pair ("DesiredVersion", Long.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamPrefixListResolverTargetId"
              , String.to_query v.ipam_prefix_list_resolver_target_id ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.track_latest_version (fun f ->
             "TrackLatestVersion", Boolean.to_json f)
       ; Aws.Util.option_map v.desired_version (fun f -> "DesiredVersion", Long.to_json f)
       ; Some
           ( "IpamPrefixListResolverTargetId"
           , String.to_json v.ipam_prefix_list_resolver_target_id )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_prefix_list_resolver_target_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPrefixListResolverTargetId"))
  ; desired_version =
      Aws.Util.option_map (Aws.Json.lookup j "DesiredVersion") Long.of_json
  ; track_latest_version =
      Aws.Util.option_map (Aws.Json.lookup j "TrackLatestVersion") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
