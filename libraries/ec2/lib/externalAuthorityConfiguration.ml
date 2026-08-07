open Aws.BaseTypes

type t =
  { type_ : IpamScopeExternalAuthorityType.t option
  ; external_resource_identifier : String.t option
  }

let make ?type_ ?external_resource_identifier () = { type_; external_resource_identifier }

let parse xml =
  Some
    { type_ =
        Aws.Util.option_bind
          (Aws.Xml.member "Type" xml)
          IpamScopeExternalAuthorityType.parse
    ; external_resource_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ExternalResourceIdentifier" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.external_resource_identifier (fun f ->
             Aws.Query.Pair ("ExternalResourceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", IpamScopeExternalAuthorityType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.external_resource_identifier (fun f ->
             "ExternalResourceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f ->
             "Type", IpamScopeExternalAuthorityType.to_json f)
       ])

let of_json j =
  { type_ =
      Aws.Util.option_map
        (Aws.Json.lookup j "Type")
        IpamScopeExternalAuthorityType.of_json
  ; external_resource_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ExternalResourceIdentifier") String.of_json
  }
