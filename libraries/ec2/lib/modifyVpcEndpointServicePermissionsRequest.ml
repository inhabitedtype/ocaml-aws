open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; service_id : String.t
  ; add_allowed_principals : ValueStringList.t
  ; remove_allowed_principals : ValueStringList.t
  }

let make
    ?dry_run
    ~service_id
    ?(add_allowed_principals = [])
    ?(remove_allowed_principals = [])
    () =
  { dry_run; service_id; add_allowed_principals; remove_allowed_principals }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; service_id =
        Aws.Xml.required
          "ServiceId"
          (Aws.Util.option_bind (Aws.Xml.member "ServiceId" xml) String.parse)
    ; add_allowed_principals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddAllowedPrincipals" xml)
             ValueStringList.parse)
    ; remove_allowed_principals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveAllowedPrincipals" xml)
             ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RemoveAllowedPrincipals"
              , ValueStringList.to_query v.remove_allowed_principals ))
       ; Some
           (Aws.Query.Pair
              ("AddAllowedPrincipals", ValueStringList.to_query v.add_allowed_principals))
       ; Some (Aws.Query.Pair ("ServiceId", String.to_query v.service_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("RemoveAllowedPrincipals", ValueStringList.to_json v.remove_allowed_principals)
       ; Some ("AddAllowedPrincipals", ValueStringList.to_json v.add_allowed_principals)
       ; Some ("ServiceId", String.to_json v.service_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; service_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceId"))
  ; add_allowed_principals =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddAllowedPrincipals"))
  ; remove_allowed_principals =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveAllowedPrincipals"))
  }
