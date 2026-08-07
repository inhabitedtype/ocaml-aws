open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_scope_id : String.t
  ; description : String.t option
  ; external_authority_configuration : ExternalAuthorityConfiguration.t option
  ; remove_external_authority_configuration : Boolean.t option
  }

let make
    ?dry_run
    ~ipam_scope_id
    ?description
    ?external_authority_configuration
    ?remove_external_authority_configuration
    () =
  { dry_run
  ; ipam_scope_id
  ; description
  ; external_authority_configuration
  ; remove_external_authority_configuration
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_scope_id =
        Aws.Xml.required
          "IpamScopeId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamScopeId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; external_authority_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ExternalAuthorityConfiguration" xml)
          ExternalAuthorityConfiguration.parse
    ; remove_external_authority_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RemoveExternalAuthorityConfiguration" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remove_external_authority_configuration (fun f ->
             Aws.Query.Pair ("RemoveExternalAuthorityConfiguration", Boolean.to_query f))
       ; Aws.Util.option_map v.external_authority_configuration (fun f ->
             Aws.Query.Pair
               ( "ExternalAuthorityConfiguration"
               , ExternalAuthorityConfiguration.to_query f ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamScopeId", String.to_query v.ipam_scope_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remove_external_authority_configuration (fun f ->
             "RemoveExternalAuthorityConfiguration", Boolean.to_json f)
       ; Aws.Util.option_map v.external_authority_configuration (fun f ->
             "ExternalAuthorityConfiguration", ExternalAuthorityConfiguration.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamScopeId", String.to_json v.ipam_scope_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_scope_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamScopeId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; external_authority_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExternalAuthorityConfiguration")
        ExternalAuthorityConfiguration.of_json
  ; remove_external_authority_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RemoveExternalAuthorityConfiguration")
        Boolean.of_json
  }
