open Aws.BaseTypes

type t =
  { organizations_access : Boolean.t
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  }

let make ~organizations_access ?dry_run ?client_token () =
  { organizations_access; dry_run; client_token }

let parse xml =
  Some
    { organizations_access =
        Aws.Xml.required
          "OrganizationsAccess"
          (Aws.Util.option_bind (Aws.Xml.member "OrganizationsAccess" xml) Boolean.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("OrganizationsAccess", Boolean.to_query v.organizations_access))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("OrganizationsAccess", Boolean.to_json v.organizations_access)
       ])

let of_json j =
  { organizations_access =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationsAccess"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
