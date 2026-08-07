open Aws.BaseTypes

type t =
  { global_cluster_identifier : String.t
  ; target_db_cluster_identifier : String.t
  ; allow_data_loss : Boolean.t option
  ; switchover : Boolean.t option
  }

let make
    ~global_cluster_identifier
    ~target_db_cluster_identifier
    ?allow_data_loss
    ?switchover
    () =
  { global_cluster_identifier; target_db_cluster_identifier; allow_data_loss; switchover }

let parse xml =
  Some
    { global_cluster_identifier =
        Aws.Xml.required
          "GlobalClusterIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalClusterIdentifier" xml)
             String.parse)
    ; target_db_cluster_identifier =
        Aws.Xml.required
          "TargetDbClusterIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetDbClusterIdentifier" xml)
             String.parse)
    ; allow_data_loss =
        Aws.Util.option_bind (Aws.Xml.member "AllowDataLoss" xml) Boolean.parse
    ; switchover = Aws.Util.option_bind (Aws.Xml.member "Switchover" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.switchover (fun f ->
             Aws.Query.Pair ("Switchover", Boolean.to_query f))
       ; Aws.Util.option_map v.allow_data_loss (fun f ->
             Aws.Query.Pair ("AllowDataLoss", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TargetDbClusterIdentifier", String.to_query v.target_db_cluster_identifier))
       ; Some
           (Aws.Query.Pair
              ("GlobalClusterIdentifier", String.to_query v.global_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.switchover (fun f -> "Switchover", Boolean.to_json f)
       ; Aws.Util.option_map v.allow_data_loss (fun f ->
             "AllowDataLoss", Boolean.to_json f)
       ; Some ("TargetDbClusterIdentifier", String.to_json v.target_db_cluster_identifier)
       ; Some ("GlobalClusterIdentifier", String.to_json v.global_cluster_identifier)
       ])

let of_json j =
  { global_cluster_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalClusterIdentifier"))
  ; target_db_cluster_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetDbClusterIdentifier"))
  ; allow_data_loss =
      Aws.Util.option_map (Aws.Json.lookup j "AllowDataLoss") Boolean.of_json
  ; switchover = Aws.Util.option_map (Aws.Json.lookup j "Switchover") Boolean.of_json
  }
