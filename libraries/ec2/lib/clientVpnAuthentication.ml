type t =
  { type_ : ClientVpnAuthenticationType.t option
  ; active_directory : DirectoryServiceAuthentication.t option
  ; mutual_authentication : CertificateAuthentication.t option
  ; federated_authentication : FederatedAuthentication.t option
  }

let make ?type_ ?active_directory ?mutual_authentication ?federated_authentication () =
  { type_; active_directory; mutual_authentication; federated_authentication }

let parse xml =
  Some
    { type_ =
        Aws.Util.option_bind (Aws.Xml.member "type" xml) ClientVpnAuthenticationType.parse
    ; active_directory =
        Aws.Util.option_bind
          (Aws.Xml.member "activeDirectory" xml)
          DirectoryServiceAuthentication.parse
    ; mutual_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "mutualAuthentication" xml)
          CertificateAuthentication.parse
    ; federated_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "federatedAuthentication" xml)
          FederatedAuthentication.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.federated_authentication (fun f ->
             Aws.Query.Pair ("FederatedAuthentication", FederatedAuthentication.to_query f))
       ; Aws.Util.option_map v.mutual_authentication (fun f ->
             Aws.Query.Pair ("MutualAuthentication", CertificateAuthentication.to_query f))
       ; Aws.Util.option_map v.active_directory (fun f ->
             Aws.Query.Pair ("ActiveDirectory", DirectoryServiceAuthentication.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ClientVpnAuthenticationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.federated_authentication (fun f ->
             "federatedAuthentication", FederatedAuthentication.to_json f)
       ; Aws.Util.option_map v.mutual_authentication (fun f ->
             "mutualAuthentication", CertificateAuthentication.to_json f)
       ; Aws.Util.option_map v.active_directory (fun f ->
             "activeDirectory", DirectoryServiceAuthentication.to_json f)
       ; Aws.Util.option_map v.type_ (fun f ->
             "type", ClientVpnAuthenticationType.to_json f)
       ])

let of_json j =
  { type_ =
      Aws.Util.option_map (Aws.Json.lookup j "type") ClientVpnAuthenticationType.of_json
  ; active_directory =
      Aws.Util.option_map
        (Aws.Json.lookup j "activeDirectory")
        DirectoryServiceAuthentication.of_json
  ; mutual_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "mutualAuthentication")
        CertificateAuthentication.of_json
  ; federated_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "federatedAuthentication")
        FederatedAuthentication.of_json
  }
