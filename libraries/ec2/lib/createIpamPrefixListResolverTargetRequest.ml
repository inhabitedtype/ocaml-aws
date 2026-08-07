open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_prefix_list_resolver_id : String.t
  ; prefix_list_id : String.t
  ; prefix_list_region : String.t
  ; desired_version : Long.t option
  ; track_latest_version : Boolean.t
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  }

let make
    ?dry_run
    ~ipam_prefix_list_resolver_id
    ~prefix_list_id
    ~prefix_list_region
    ?desired_version
    ~track_latest_version
    ?(tag_specifications = [])
    ?client_token
    () =
  { dry_run
  ; ipam_prefix_list_resolver_id
  ; prefix_list_id
  ; prefix_list_region
  ; desired_version
  ; track_latest_version
  ; tag_specifications
  ; client_token
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_prefix_list_resolver_id =
        Aws.Xml.required
          "IpamPrefixListResolverId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamPrefixListResolverId" xml)
             String.parse)
    ; prefix_list_id =
        Aws.Xml.required
          "PrefixListId"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListId" xml) String.parse)
    ; prefix_list_region =
        Aws.Xml.required
          "PrefixListRegion"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListRegion" xml) String.parse)
    ; desired_version =
        Aws.Util.option_bind (Aws.Xml.member "DesiredVersion" xml) Long.parse
    ; track_latest_version =
        Aws.Xml.required
          "TrackLatestVersion"
          (Aws.Util.option_bind (Aws.Xml.member "TrackLatestVersion" xml) Boolean.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair ("TrackLatestVersion", Boolean.to_query v.track_latest_version))
       ; Aws.Util.option_map v.desired_version (fun f ->
             Aws.Query.Pair ("DesiredVersion", Long.to_query f))
       ; Some (Aws.Query.Pair ("PrefixListRegion", String.to_query v.prefix_list_region))
       ; Some (Aws.Query.Pair ("PrefixListId", String.to_query v.prefix_list_id))
       ; Some
           (Aws.Query.Pair
              ("IpamPrefixListResolverId", String.to_query v.ipam_prefix_list_resolver_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("TrackLatestVersion", Boolean.to_json v.track_latest_version)
       ; Aws.Util.option_map v.desired_version (fun f -> "DesiredVersion", Long.to_json f)
       ; Some ("PrefixListRegion", String.to_json v.prefix_list_region)
       ; Some ("PrefixListId", String.to_json v.prefix_list_id)
       ; Some ("IpamPrefixListResolverId", String.to_json v.ipam_prefix_list_resolver_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_prefix_list_resolver_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPrefixListResolverId"))
  ; prefix_list_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListId"))
  ; prefix_list_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListRegion"))
  ; desired_version =
      Aws.Util.option_map (Aws.Json.lookup j "DesiredVersion") Long.of_json
  ; track_latest_version =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrackLatestVersion"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
