open Aws.BaseTypes

type t =
  { audience : WebIdentityTokenAudienceListType.t
  ; duration_seconds : Integer.t option
  ; signing_algorithm : String.t
  ; tags : TagListType.t
  }

let make ~audience ?duration_seconds ~signing_algorithm ?(tags = []) () =
  { audience; duration_seconds; signing_algorithm; tags }

let parse xml =
  Some
    { audience =
        Aws.Xml.required
          "Audience"
          (Aws.Util.option_bind
             (Aws.Xml.member "Audience" xml)
             WebIdentityTokenAudienceListType.parse)
    ; duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
    ; signing_algorithm =
        Aws.Xml.required
          "SigningAlgorithm"
          (Aws.Util.option_bind (Aws.Xml.member "SigningAlgorithm" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagListType.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagListType.to_query v.tags))
       ; Some (Aws.Query.Pair ("SigningAlgorithm", String.to_query v.signing_algorithm))
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Audience.member", WebIdentityTokenAudienceListType.to_query v.audience))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagListType.to_json v.tags)
       ; Some ("SigningAlgorithm", String.to_json v.signing_algorithm)
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ; Some ("Audience", WebIdentityTokenAudienceListType.to_json v.audience)
       ])

let of_json j =
  { audience =
      WebIdentityTokenAudienceListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Audience"))
  ; duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  ; signing_algorithm =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SigningAlgorithm"))
  ; tags = TagListType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
