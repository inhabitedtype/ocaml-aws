open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t
  ; target_d_b_instance_identifier : String.t option
  }

let make ~d_b_cluster_identifier ?target_d_b_instance_identifier () =
  { d_b_cluster_identifier; target_d_b_instance_identifier }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; target_d_b_instance_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetDBInstanceIdentifier" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("TargetDBInstanceIdentifier", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_d_b_instance_identifier (fun f ->
             "TargetDBInstanceIdentifier", String.to_json f)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ])

let of_json j =
  { d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; target_d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "TargetDBInstanceIdentifier") String.of_json
  }
