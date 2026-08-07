open Aws.BaseTypes

type t =
  { region_name : String.t option
  ; endpoint : String.t option
  ; status : String.t option
  ; supports_d_b_instance_automated_backups_replication : Boolean.t option
  }

let make
    ?region_name
    ?endpoint
    ?status
    ?supports_d_b_instance_automated_backups_replication
    () =
  { region_name; endpoint; status; supports_d_b_instance_automated_backups_replication }

let parse xml =
  Some
    { region_name = Aws.Util.option_bind (Aws.Xml.member "RegionName" xml) String.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; supports_d_b_instance_automated_backups_replication =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsDBInstanceAutomatedBackupsReplication" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map
           v.supports_d_b_instance_automated_backups_replication
           (fun f ->
             Aws.Query.Pair
               ("SupportsDBInstanceAutomatedBackupsReplication", Boolean.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Aws.Util.option_map v.region_name (fun f ->
             Aws.Query.Pair ("RegionName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map
           v.supports_d_b_instance_automated_backups_replication
           (fun f -> "SupportsDBInstanceAutomatedBackupsReplication", Boolean.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Aws.Util.option_map v.region_name (fun f -> "RegionName", String.to_json f)
       ])

let of_json j =
  { region_name = Aws.Util.option_map (Aws.Json.lookup j "RegionName") String.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; supports_d_b_instance_automated_backups_replication =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsDBInstanceAutomatedBackupsReplication")
        Boolean.of_json
  }
