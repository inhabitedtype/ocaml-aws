type t =
  { igmpv2_support : Igmpv2SupportValue.t option
  ; static_sources_support : StaticSourcesSupportValue.t option
  ; auto_accept_shared_associations : AutoAcceptSharedAssociationsValue.t option
  }

let make ?igmpv2_support ?static_sources_support ?auto_accept_shared_associations () =
  { igmpv2_support; static_sources_support; auto_accept_shared_associations }

let parse xml =
  Some
    { igmpv2_support =
        Aws.Util.option_bind (Aws.Xml.member "igmpv2Support" xml) Igmpv2SupportValue.parse
    ; static_sources_support =
        Aws.Util.option_bind
          (Aws.Xml.member "staticSourcesSupport" xml)
          StaticSourcesSupportValue.parse
    ; auto_accept_shared_associations =
        Aws.Util.option_bind
          (Aws.Xml.member "autoAcceptSharedAssociations" xml)
          AutoAcceptSharedAssociationsValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_accept_shared_associations (fun f ->
             Aws.Query.Pair
               ( "AutoAcceptSharedAssociations"
               , AutoAcceptSharedAssociationsValue.to_query f ))
       ; Aws.Util.option_map v.static_sources_support (fun f ->
             Aws.Query.Pair ("StaticSourcesSupport", StaticSourcesSupportValue.to_query f))
       ; Aws.Util.option_map v.igmpv2_support (fun f ->
             Aws.Query.Pair ("Igmpv2Support", Igmpv2SupportValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_accept_shared_associations (fun f ->
             "autoAcceptSharedAssociations", AutoAcceptSharedAssociationsValue.to_json f)
       ; Aws.Util.option_map v.static_sources_support (fun f ->
             "staticSourcesSupport", StaticSourcesSupportValue.to_json f)
       ; Aws.Util.option_map v.igmpv2_support (fun f ->
             "igmpv2Support", Igmpv2SupportValue.to_json f)
       ])

let of_json j =
  { igmpv2_support =
      Aws.Util.option_map (Aws.Json.lookup j "igmpv2Support") Igmpv2SupportValue.of_json
  ; static_sources_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "staticSourcesSupport")
        StaticSourcesSupportValue.of_json
  ; auto_accept_shared_associations =
      Aws.Util.option_map
        (Aws.Json.lookup j "autoAcceptSharedAssociations")
        AutoAcceptSharedAssociationsValue.of_json
  }
