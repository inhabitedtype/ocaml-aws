open Aws.BaseTypes

type t =
  { status : FailoverStatus.t option
  ; from_db_cluster_arn : String.t option
  ; to_db_cluster_arn : String.t option
  ; is_data_loss_allowed : Boolean.t option
  }

let make ?status ?from_db_cluster_arn ?to_db_cluster_arn ?is_data_loss_allowed () =
  { status; from_db_cluster_arn; to_db_cluster_arn; is_data_loss_allowed }

let parse xml =
  Some
    { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) FailoverStatus.parse
    ; from_db_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "FromDbClusterArn" xml) String.parse
    ; to_db_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "ToDbClusterArn" xml) String.parse
    ; is_data_loss_allowed =
        Aws.Util.option_bind (Aws.Xml.member "IsDataLossAllowed" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_data_loss_allowed (fun f ->
             Aws.Query.Pair ("IsDataLossAllowed", Boolean.to_query f))
       ; Aws.Util.option_map v.to_db_cluster_arn (fun f ->
             Aws.Query.Pair ("ToDbClusterArn", String.to_query f))
       ; Aws.Util.option_map v.from_db_cluster_arn (fun f ->
             Aws.Query.Pair ("FromDbClusterArn", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", FailoverStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_data_loss_allowed (fun f ->
             "IsDataLossAllowed", Boolean.to_json f)
       ; Aws.Util.option_map v.to_db_cluster_arn (fun f ->
             "ToDbClusterArn", String.to_json f)
       ; Aws.Util.option_map v.from_db_cluster_arn (fun f ->
             "FromDbClusterArn", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", FailoverStatus.to_json f)
       ])

let of_json j =
  { status = Aws.Util.option_map (Aws.Json.lookup j "Status") FailoverStatus.of_json
  ; from_db_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "FromDbClusterArn") String.of_json
  ; to_db_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ToDbClusterArn") String.of_json
  ; is_data_loss_allowed =
      Aws.Util.option_map (Aws.Json.lookup j "IsDataLossAllowed") Boolean.of_json
  }
