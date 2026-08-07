open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; grant_id : String.t option
  ; name : String.t option
  ; creation_date : DateTime.t option
  ; grantee_principal : String.t option
  ; retiring_principal : String.t option
  ; issuing_account : String.t option
  ; operations : GrantOperationList.t
  ; constraints : GrantConstraints.t option
  ; grantee_service_principal : String.t option
  ; retiring_service_principal : String.t option
  }

let make
    ?key_id
    ?grant_id
    ?name
    ?creation_date
    ?grantee_principal
    ?retiring_principal
    ?issuing_account
    ?(operations = [])
    ?constraints
    ?grantee_service_principal
    ?retiring_service_principal
    () =
  { key_id
  ; grant_id
  ; name
  ; creation_date
  ; grantee_principal
  ; retiring_principal
  ; issuing_account
  ; operations
  ; constraints
  ; grantee_service_principal
  ; retiring_service_principal
  }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; creation_date =
        Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
    ; grantee_principal =
        Aws.Util.option_bind (Aws.Xml.member "GranteePrincipal" xml) String.parse
    ; retiring_principal =
        Aws.Util.option_bind (Aws.Xml.member "RetiringPrincipal" xml) String.parse
    ; issuing_account =
        Aws.Util.option_bind (Aws.Xml.member "IssuingAccount" xml) String.parse
    ; operations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Operations" xml)
             GrantOperationList.parse)
    ; constraints =
        Aws.Util.option_bind (Aws.Xml.member "Constraints" xml) GrantConstraints.parse
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
       ; Aws.Util.option_map v.constraints (fun f ->
             Aws.Query.Pair ("Constraints", GrantConstraints.to_query f))
       ; Some
           (Aws.Query.Pair ("Operations.member", GrantOperationList.to_query v.operations))
       ; Aws.Util.option_map v.issuing_account (fun f ->
             Aws.Query.Pair ("IssuingAccount", String.to_query f))
       ; Aws.Util.option_map v.retiring_principal (fun f ->
             Aws.Query.Pair ("RetiringPrincipal", String.to_query f))
       ; Aws.Util.option_map v.grantee_principal (fun f ->
             Aws.Query.Pair ("GranteePrincipal", String.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.grant_id (fun f ->
             Aws.Query.Pair ("GrantId", String.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retiring_service_principal (fun f ->
             "RetiringServicePrincipal", String.to_json f)
       ; Aws.Util.option_map v.grantee_service_principal (fun f ->
             "GranteeServicePrincipal", String.to_json f)
       ; Aws.Util.option_map v.constraints (fun f ->
             "Constraints", GrantConstraints.to_json f)
       ; Some ("Operations", GrantOperationList.to_json v.operations)
       ; Aws.Util.option_map v.issuing_account (fun f ->
             "IssuingAccount", String.to_json f)
       ; Aws.Util.option_map v.retiring_principal (fun f ->
             "RetiringPrincipal", String.to_json f)
       ; Aws.Util.option_map v.grantee_principal (fun f ->
             "GranteePrincipal", String.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "CreationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; creation_date =
      Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
  ; grantee_principal =
      Aws.Util.option_map (Aws.Json.lookup j "GranteePrincipal") String.of_json
  ; retiring_principal =
      Aws.Util.option_map (Aws.Json.lookup j "RetiringPrincipal") String.of_json
  ; issuing_account =
      Aws.Util.option_map (Aws.Json.lookup j "IssuingAccount") String.of_json
  ; operations =
      GrantOperationList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Operations"))
  ; constraints =
      Aws.Util.option_map (Aws.Json.lookup j "Constraints") GrantConstraints.of_json
  ; grantee_service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "GranteeServicePrincipal") String.of_json
  ; retiring_service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "RetiringServicePrincipal") String.of_json
  }
