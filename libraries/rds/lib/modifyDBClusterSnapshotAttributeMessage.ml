open Aws.BaseTypes

type t =
  { d_b_cluster_snapshot_identifier : String.t
  ; attribute_name : String.t
  ; values_to_add : AttributeValueList.t
  ; values_to_remove : AttributeValueList.t
  }

let make
    ~d_b_cluster_snapshot_identifier
    ~attribute_name
    ?(values_to_add = [])
    ?(values_to_remove = [])
    () =
  { d_b_cluster_snapshot_identifier; attribute_name; values_to_add; values_to_remove }

let parse xml =
  Some
    { d_b_cluster_snapshot_identifier =
        Aws.Xml.required
          "DBClusterSnapshotIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterSnapshotIdentifier" xml)
             String.parse)
    ; attribute_name =
        Aws.Xml.required
          "AttributeName"
          (Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse)
    ; values_to_add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ValuesToAdd" xml)
             AttributeValueList.parse)
    ; values_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ValuesToRemove" xml)
             AttributeValueList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ValuesToRemove.member", AttributeValueList.to_query v.values_to_remove))
       ; Some
           (Aws.Query.Pair
              ("ValuesToAdd.member", AttributeValueList.to_query v.values_to_add))
       ; Some (Aws.Query.Pair ("AttributeName", String.to_query v.attribute_name))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterSnapshotIdentifier"
              , String.to_query v.d_b_cluster_snapshot_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ValuesToRemove", AttributeValueList.to_json v.values_to_remove)
       ; Some ("ValuesToAdd", AttributeValueList.to_json v.values_to_add)
       ; Some ("AttributeName", String.to_json v.attribute_name)
       ; Some
           ( "DBClusterSnapshotIdentifier"
           , String.to_json v.d_b_cluster_snapshot_identifier )
       ])

let of_json j =
  { d_b_cluster_snapshot_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterSnapshotIdentifier"))
  ; attribute_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeName"))
  ; values_to_add =
      AttributeValueList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ValuesToAdd"))
  ; values_to_remove =
      AttributeValueList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ValuesToRemove"))
  }
