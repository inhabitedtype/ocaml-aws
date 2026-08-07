open Aws.BaseTypes

type t =
  { source_d_b_cluster_parameter_group_identifier : String.t
  ; target_d_b_cluster_parameter_group_identifier : String.t
  ; target_d_b_cluster_parameter_group_description : String.t
  ; tags : TagList.t
  }

let make
    ~source_d_b_cluster_parameter_group_identifier
    ~target_d_b_cluster_parameter_group_identifier
    ~target_d_b_cluster_parameter_group_description
    ?(tags = [])
    () =
  { source_d_b_cluster_parameter_group_identifier
  ; target_d_b_cluster_parameter_group_identifier
  ; target_d_b_cluster_parameter_group_description
  ; tags
  }

let parse xml =
  Some
    { source_d_b_cluster_parameter_group_identifier =
        Aws.Xml.required
          "SourceDBClusterParameterGroupIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceDBClusterParameterGroupIdentifier" xml)
             String.parse)
    ; target_d_b_cluster_parameter_group_identifier =
        Aws.Xml.required
          "TargetDBClusterParameterGroupIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetDBClusterParameterGroupIdentifier" xml)
             String.parse)
    ; target_d_b_cluster_parameter_group_description =
        Aws.Xml.required
          "TargetDBClusterParameterGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetDBClusterParameterGroupDescription" xml)
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
              ( "TargetDBClusterParameterGroupDescription"
              , String.to_query v.target_d_b_cluster_parameter_group_description ))
       ; Some
           (Aws.Query.Pair
              ( "TargetDBClusterParameterGroupIdentifier"
              , String.to_query v.target_d_b_cluster_parameter_group_identifier ))
       ; Some
           (Aws.Query.Pair
              ( "SourceDBClusterParameterGroupIdentifier"
              , String.to_query v.source_d_b_cluster_parameter_group_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some
           ( "TargetDBClusterParameterGroupDescription"
           , String.to_json v.target_d_b_cluster_parameter_group_description )
       ; Some
           ( "TargetDBClusterParameterGroupIdentifier"
           , String.to_json v.target_d_b_cluster_parameter_group_identifier )
       ; Some
           ( "SourceDBClusterParameterGroupIdentifier"
           , String.to_json v.source_d_b_cluster_parameter_group_identifier )
       ])

let of_json j =
  { source_d_b_cluster_parameter_group_identifier =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "SourceDBClusterParameterGroupIdentifier"))
  ; target_d_b_cluster_parameter_group_identifier =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "TargetDBClusterParameterGroupIdentifier"))
  ; target_d_b_cluster_parameter_group_description =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "TargetDBClusterParameterGroupDescription"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
