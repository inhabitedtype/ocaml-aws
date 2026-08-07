open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; prefix_list_id : String.t
  ; current_version : Long.t option
  ; prefix_list_name : String.t option
  ; add_entries : AddPrefixListEntries.t
  ; remove_entries : RemovePrefixListEntries.t
  ; max_entries : Integer.t option
  ; ipam_prefix_list_resolver_sync_enabled : Boolean.t option
  }

let make
    ?dry_run
    ~prefix_list_id
    ?current_version
    ?prefix_list_name
    ?(add_entries = [])
    ?(remove_entries = [])
    ?max_entries
    ?ipam_prefix_list_resolver_sync_enabled
    () =
  { dry_run
  ; prefix_list_id
  ; current_version
  ; prefix_list_name
  ; add_entries
  ; remove_entries
  ; max_entries
  ; ipam_prefix_list_resolver_sync_enabled
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; prefix_list_id =
        Aws.Xml.required
          "PrefixListId"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListId" xml) String.parse)
    ; current_version =
        Aws.Util.option_bind (Aws.Xml.member "CurrentVersion" xml) Long.parse
    ; prefix_list_name =
        Aws.Util.option_bind (Aws.Xml.member "PrefixListName" xml) String.parse
    ; add_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddEntry" xml)
             AddPrefixListEntries.parse)
    ; remove_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveEntry" xml)
             RemovePrefixListEntries.parse)
    ; max_entries = Aws.Util.option_bind (Aws.Xml.member "MaxEntries" xml) Integer.parse
    ; ipam_prefix_list_resolver_sync_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IpamPrefixListResolverSyncEnabled" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver_sync_enabled (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverSyncEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.max_entries (fun f ->
             Aws.Query.Pair ("MaxEntries", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RemoveEntry", RemovePrefixListEntries.to_query v.remove_entries))
       ; Some (Aws.Query.Pair ("AddEntry", AddPrefixListEntries.to_query v.add_entries))
       ; Aws.Util.option_map v.prefix_list_name (fun f ->
             Aws.Query.Pair ("PrefixListName", String.to_query f))
       ; Aws.Util.option_map v.current_version (fun f ->
             Aws.Query.Pair ("CurrentVersion", Long.to_query f))
       ; Some (Aws.Query.Pair ("PrefixListId", String.to_query v.prefix_list_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver_sync_enabled (fun f ->
             "IpamPrefixListResolverSyncEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.max_entries (fun f -> "MaxEntries", Integer.to_json f)
       ; Some ("RemoveEntry", RemovePrefixListEntries.to_json v.remove_entries)
       ; Some ("AddEntry", AddPrefixListEntries.to_json v.add_entries)
       ; Aws.Util.option_map v.prefix_list_name (fun f ->
             "PrefixListName", String.to_json f)
       ; Aws.Util.option_map v.current_version (fun f -> "CurrentVersion", Long.to_json f)
       ; Some ("PrefixListId", String.to_json v.prefix_list_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; prefix_list_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListId"))
  ; current_version =
      Aws.Util.option_map (Aws.Json.lookup j "CurrentVersion") Long.of_json
  ; prefix_list_name =
      Aws.Util.option_map (Aws.Json.lookup j "PrefixListName") String.of_json
  ; add_entries =
      AddPrefixListEntries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AddEntry"))
  ; remove_entries =
      RemovePrefixListEntries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveEntry"))
  ; max_entries = Aws.Util.option_map (Aws.Json.lookup j "MaxEntries") Integer.of_json
  ; ipam_prefix_list_resolver_sync_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IpamPrefixListResolverSyncEnabled")
        Boolean.of_json
  }
