open Aws.BaseTypes

type t =
  { source_option_group_identifier : String.t
  ; target_option_group_identifier : String.t
  ; target_option_group_description : String.t
  ; tags : TagList.t
  }

let make
    ~source_option_group_identifier
    ~target_option_group_identifier
    ~target_option_group_description
    ?(tags = [])
    () =
  { source_option_group_identifier
  ; target_option_group_identifier
  ; target_option_group_description
  ; tags
  }

let parse xml =
  Some
    { source_option_group_identifier =
        Aws.Xml.required
          "SourceOptionGroupIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceOptionGroupIdentifier" xml)
             String.parse)
    ; target_option_group_identifier =
        Aws.Xml.required
          "TargetOptionGroupIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetOptionGroupIdentifier" xml)
             String.parse)
    ; target_option_group_description =
        Aws.Xml.required
          "TargetOptionGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetOptionGroupDescription" xml)
             String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "TargetOptionGroupDescription"
              , String.to_query v.target_option_group_description ))
       ; Some
           (Aws.Query.Pair
              ( "TargetOptionGroupIdentifier"
              , String.to_query v.target_option_group_identifier ))
       ; Some
           (Aws.Query.Pair
              ( "SourceOptionGroupIdentifier"
              , String.to_query v.source_option_group_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some
           ( "TargetOptionGroupDescription"
           , String.to_json v.target_option_group_description )
       ; Some
           ("TargetOptionGroupIdentifier", String.to_json v.target_option_group_identifier)
       ; Some
           ("SourceOptionGroupIdentifier", String.to_json v.source_option_group_identifier)
       ])

let of_json j =
  { source_option_group_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceOptionGroupIdentifier"))
  ; target_option_group_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetOptionGroupIdentifier"))
  ; target_option_group_description =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetOptionGroupDescription"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
