type t =
  { type_ : ClientVpnAuthenticationType.t option
  ; active_directory : DirectoryServiceAuthenticationRequest.t option
  ; mutual_authentication : CertificateAuthenticationRequest.t option
  ; federated_authentication : FederatedAuthenticationRequest.t option
  }

let make ?type_ ?active_directory ?mutual_authentication ?federated_authentication () =
  { type_; active_directory; mutual_authentication; federated_authentication }

let parse xml =
  Some
    { type_ =
        Aws.Util.option_bind (Aws.Xml.member "Type" xml) ClientVpnAuthenticationType.parse
    ; active_directory =
        Aws.Util.option_bind
          (Aws.Xml.member "ActiveDirectory" xml)
          DirectoryServiceAuthenticationRequest.parse
    ; mutual_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "MutualAuthentication" xml)
          CertificateAuthenticationRequest.parse
    ; federated_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "FederatedAuthentication" xml)
          FederatedAuthenticationRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.federated_authentication (fun f ->
             Aws.Query.Pair
               ("FederatedAuthentication", FederatedAuthenticationRequest.to_query f))
       ; Aws.Util.option_map v.mutual_authentication (fun f ->
             Aws.Query.Pair
               ("MutualAuthentication", CertificateAuthenticationRequest.to_query f))
       ; Aws.Util.option_map v.active_directory (fun f ->
             Aws.Query.Pair
               ("ActiveDirectory", DirectoryServiceAuthenticationRequest.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ClientVpnAuthenticationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.federated_authentication (fun f ->
             "FederatedAuthentication", FederatedAuthenticationRequest.to_json f)
       ; Aws.Util.option_map v.mutual_authentication (fun f ->
             "MutualAuthentication", CertificateAuthenticationRequest.to_json f)
       ; Aws.Util.option_map v.active_directory (fun f ->
             "ActiveDirectory", DirectoryServiceAuthenticationRequest.to_json f)
       ; Aws.Util.option_map v.type_ (fun f ->
             "Type", ClientVpnAuthenticationType.to_json f)
       ])

let of_json j =
  { type_ =
      Aws.Util.option_map (Aws.Json.lookup j "Type") ClientVpnAuthenticationType.of_json
  ; active_directory =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActiveDirectory")
        DirectoryServiceAuthenticationRequest.of_json
  ; mutual_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "MutualAuthentication")
        CertificateAuthenticationRequest.of_json
  ; federated_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "FederatedAuthentication")
        FederatedAuthenticationRequest.of_json
  }
