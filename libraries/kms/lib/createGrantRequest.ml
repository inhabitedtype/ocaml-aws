open Aws.BaseTypes

type t =
  { key_id : String.t
  ; grantee_principal : String.t option
  ; retiring_principal : String.t option
  ; operations : GrantOperationList.t
  ; constraints : GrantConstraints.t option
  ; grant_tokens : GrantTokenList.t
  ; name : String.t option
  ; dry_run : Boolean.t option
  ; grantee_service_principal : String.t option
  ; retiring_service_principal : String.t option
  }

let make
    ~key_id
    ?grantee_principal
    ?retiring_principal
    ~operations
    ?constraints
    ?(grant_tokens = [])
    ?name
    ?dry_run
    ?grantee_service_principal
    ?retiring_service_principal
    () =
  { key_id
  ; grantee_principal
  ; retiring_principal
  ; operations
  ; constraints
  ; grant_tokens
  ; name
  ; dry_run
  ; grantee_service_principal
  ; retiring_service_principal
  }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; grantee_principal =
        Aws.Util.option_bind (Aws.Xml.member "GranteePrincipal" xml) String.parse
    ; retiring_principal =
        Aws.Util.option_bind (Aws.Xml.member "RetiringPrincipal" xml) String.parse
    ; operations =
        Aws.Xml.required
          "Operations"
          (Aws.Util.option_bind
             (Aws.Xml.member "Operations" xml)
             GrantOperationList.parse)
    ; constraints =
        Aws.Util.option_bind (Aws.Xml.member "Constraints" xml) GrantConstraints.parse
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; grantee_service_principal =
        Aws.Util.option_bind (Aws.Xml.member "GranteeServicePrincipal" xml) String.parse
    ; retiring_service_principal =
        Aws.Util.option_bind (Aws.Xml.member "RetiringServicePrincipal" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retiring_service_principal (fun f ->
             Aws.Query.Pair ("RetiringServicePrincipal", String.to_query f))
       ; Aws.Util.option_map v.grantee_service_principal (fun f ->
             Aws.Query.Pair ("GranteeServicePrincipal", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Aws.Util.option_map v.constraints (fun f ->
             Aws.Query.Pair ("Constraints", GrantConstraints.to_query f))
       ; Some
           (Aws.Query.Pair ("Operations.member", GrantOperationList.to_query v.operations))
       ; Aws.Util.option_map v.retiring_principal (fun f ->
             Aws.Query.Pair ("RetiringPrincipal", String.to_query f))
       ; Aws.Util.option_map v.grantee_principal (fun f ->
             Aws.Query.Pair ("GranteePrincipal", String.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retiring_service_principal (fun f ->
             "RetiringServicePrincipal", String.to_json f)
       ; Aws.Util.option_map v.grantee_service_principal (fun f ->
             "GranteeServicePrincipal", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Aws.Util.option_map v.constraints (fun f ->
             "Constraints", GrantConstraints.to_json f)
       ; Some ("Operations", GrantOperationList.to_json v.operations)
       ; Aws.Util.option_map v.retiring_principal (fun f ->
             "RetiringPrincipal", String.to_json f)
       ; Aws.Util.option_map v.grantee_principal (fun f ->
             "GranteePrincipal", String.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; grantee_principal =
      Aws.Util.option_map (Aws.Json.lookup j "GranteePrincipal") String.of_json
  ; retiring_principal =
      Aws.Util.option_map (Aws.Json.lookup j "RetiringPrincipal") String.of_json
  ; operations =
      GrantOperationList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Operations"))
  ; constraints =
      Aws.Util.option_map (Aws.Json.lookup j "Constraints") GrantConstraints.of_json
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; grantee_service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "GranteeServicePrincipal") String.of_json
  ; retiring_service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "RetiringServicePrincipal") String.of_json
  }
