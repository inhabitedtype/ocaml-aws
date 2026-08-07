open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t option
  ; cache_node_id : String.t option
  ; node_update_status : NodeUpdateStatus.t option
  ; node_deletion_date : DateTime.t option
  ; node_update_start_date : DateTime.t option
  ; node_update_end_date : DateTime.t option
  ; node_update_initiated_by : NodeUpdateInitiatedBy.t option
  ; node_update_initiated_date : DateTime.t option
  ; node_update_status_modified_date : DateTime.t option
  }

let make
    ?cache_cluster_id
    ?cache_node_id
    ?node_update_status
    ?node_deletion_date
    ?node_update_start_date
    ?node_update_end_date
    ?node_update_initiated_by
    ?node_update_initiated_date
    ?node_update_status_modified_date
    () =
  { cache_cluster_id
  ; cache_node_id
  ; node_update_status
  ; node_deletion_date
  ; node_update_start_date
  ; node_update_end_date
  ; node_update_initiated_by
  ; node_update_initiated_date
  ; node_update_status_modified_date
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; cache_node_id = Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
    ; node_update_status =
        Aws.Util.option_bind
          (Aws.Xml.member "NodeUpdateStatus" xml)
          NodeUpdateStatus.parse
    ; node_deletion_date =
        Aws.Util.option_bind (Aws.Xml.member "NodeDeletionDate" xml) DateTime.parse
    ; node_update_start_date =
        Aws.Util.option_bind (Aws.Xml.member "NodeUpdateStartDate" xml) DateTime.parse
    ; node_update_end_date =
        Aws.Util.option_bind (Aws.Xml.member "NodeUpdateEndDate" xml) DateTime.parse
    ; node_update_initiated_by =
        Aws.Util.option_bind
          (Aws.Xml.member "NodeUpdateInitiatedBy" xml)
          NodeUpdateInitiatedBy.parse
    ; node_update_initiated_date =
        Aws.Util.option_bind (Aws.Xml.member "NodeUpdateInitiatedDate" xml) DateTime.parse
    ; node_update_status_modified_date =
        Aws.Util.option_bind
          (Aws.Xml.member "NodeUpdateStatusModifiedDate" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.node_update_status_modified_date (fun f ->
             Aws.Query.Pair ("NodeUpdateStatusModifiedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.node_update_initiated_date (fun f ->
             Aws.Query.Pair ("NodeUpdateInitiatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.node_update_initiated_by (fun f ->
             Aws.Query.Pair ("NodeUpdateInitiatedBy", NodeUpdateInitiatedBy.to_query f))
       ; Aws.Util.option_map v.node_update_end_date (fun f ->
             Aws.Query.Pair ("NodeUpdateEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.node_update_start_date (fun f ->
             Aws.Query.Pair ("NodeUpdateStartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.node_deletion_date (fun f ->
             Aws.Query.Pair ("NodeDeletionDate", DateTime.to_query f))
       ; Aws.Util.option_map v.node_update_status (fun f ->
             Aws.Query.Pair ("NodeUpdateStatus", NodeUpdateStatus.to_query f))
       ; Aws.Util.option_map v.cache_node_id (fun f ->
             Aws.Query.Pair ("CacheNodeId", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.node_update_status_modified_date (fun f ->
             "NodeUpdateStatusModifiedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.node_update_initiated_date (fun f ->
             "NodeUpdateInitiatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.node_update_initiated_by (fun f ->
             "NodeUpdateInitiatedBy", NodeUpdateInitiatedBy.to_json f)
       ; Aws.Util.option_map v.node_update_end_date (fun f ->
             "NodeUpdateEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.node_update_start_date (fun f ->
             "NodeUpdateStartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.node_deletion_date (fun f ->
             "NodeDeletionDate", DateTime.to_json f)
       ; Aws.Util.option_map v.node_update_status (fun f ->
             "NodeUpdateStatus", NodeUpdateStatus.to_json f)
       ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ])

let of_json j =
  { cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
  ; node_update_status =
      Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateStatus") NodeUpdateStatus.of_json
  ; node_deletion_date =
      Aws.Util.option_map (Aws.Json.lookup j "NodeDeletionDate") DateTime.of_json
  ; node_update_start_date =
      Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateStartDate") DateTime.of_json
  ; node_update_end_date =
      Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateEndDate") DateTime.of_json
  ; node_update_initiated_by =
      Aws.Util.option_map
        (Aws.Json.lookup j "NodeUpdateInitiatedBy")
        NodeUpdateInitiatedBy.of_json
  ; node_update_initiated_date =
      Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateInitiatedDate") DateTime.of_json
  ; node_update_status_modified_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "NodeUpdateStatusModifiedDate")
        DateTime.of_json
  }
