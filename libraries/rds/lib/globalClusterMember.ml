open Aws.BaseTypes

type t =
  { d_b_cluster_arn : String.t option
  ; readers : ReadersArnList.t
  ; is_writer : Boolean.t option
  ; global_write_forwarding_status : WriteForwardingStatus.t option
  ; synchronization_status : GlobalClusterMemberSynchronizationStatus.t option
  }

let make
    ?d_b_cluster_arn
    ?(readers = [])
    ?is_writer
    ?global_write_forwarding_status
    ?synchronization_status
    () =
  { d_b_cluster_arn
  ; readers
  ; is_writer
  ; global_write_forwarding_status
  ; synchronization_status
  }

let parse xml =
  Some
    { d_b_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterArn" xml) String.parse
    ; readers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Readers" xml) ReadersArnList.parse)
    ; is_writer = Aws.Util.option_bind (Aws.Xml.member "IsWriter" xml) Boolean.parse
    ; global_write_forwarding_status =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalWriteForwardingStatus" xml)
          WriteForwardingStatus.parse
    ; synchronization_status =
        Aws.Util.option_bind
          (Aws.Xml.member "SynchronizationStatus" xml)
          GlobalClusterMemberSynchronizationStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.synchronization_status (fun f ->
             Aws.Query.Pair
               ( "SynchronizationStatus"
               , GlobalClusterMemberSynchronizationStatus.to_query f ))
       ; Aws.Util.option_map v.global_write_forwarding_status (fun f ->
             Aws.Query.Pair
               ("GlobalWriteForwardingStatus", WriteForwardingStatus.to_query f))
       ; Aws.Util.option_map v.is_writer (fun f ->
             Aws.Query.Pair ("IsWriter", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Readers.member", ReadersArnList.to_query v.readers))
       ; Aws.Util.option_map v.d_b_cluster_arn (fun f ->
             Aws.Query.Pair ("DBClusterArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.synchronization_status (fun f ->
             "SynchronizationStatus", GlobalClusterMemberSynchronizationStatus.to_json f)
       ; Aws.Util.option_map v.global_write_forwarding_status (fun f ->
             "GlobalWriteForwardingStatus", WriteForwardingStatus.to_json f)
       ; Aws.Util.option_map v.is_writer (fun f -> "IsWriter", Boolean.to_json f)
       ; Some ("Readers", ReadersArnList.to_json v.readers)
       ; Aws.Util.option_map v.d_b_cluster_arn (fun f -> "DBClusterArn", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterArn") String.of_json
  ; readers =
      ReadersArnList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Readers"))
  ; is_writer = Aws.Util.option_map (Aws.Json.lookup j "IsWriter") Boolean.of_json
  ; global_write_forwarding_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalWriteForwardingStatus")
        WriteForwardingStatus.of_json
  ; synchronization_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "SynchronizationStatus")
        GlobalClusterMemberSynchronizationStatus.of_json
  }
