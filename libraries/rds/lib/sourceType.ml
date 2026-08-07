open Aws.BaseTypes

type t =
  | Db_instance
  | Db_parameter_group
  | Db_security_group
  | Db_snapshot
  | Db_cluster
  | Db_cluster_snapshot
  | Custom_engine_version
  | Db_proxy
  | Blue_green_deployment
  | Db_shard_group
  | Zero_etl

let str_to_t =
  [ "zero-etl", Zero_etl
  ; "db-shard-group", Db_shard_group
  ; "blue-green-deployment", Blue_green_deployment
  ; "db-proxy", Db_proxy
  ; "custom-engine-version", Custom_engine_version
  ; "db-cluster-snapshot", Db_cluster_snapshot
  ; "db-cluster", Db_cluster
  ; "db-snapshot", Db_snapshot
  ; "db-security-group", Db_security_group
  ; "db-parameter-group", Db_parameter_group
  ; "db-instance", Db_instance
  ]

let t_to_str =
  [ Zero_etl, "zero-etl"
  ; Db_shard_group, "db-shard-group"
  ; Blue_green_deployment, "blue-green-deployment"
  ; Db_proxy, "db-proxy"
  ; Custom_engine_version, "custom-engine-version"
  ; Db_cluster_snapshot, "db-cluster-snapshot"
  ; Db_cluster, "db-cluster"
  ; Db_snapshot, "db-snapshot"
  ; Db_security_group, "db-security-group"
  ; Db_parameter_group, "db-parameter-group"
  ; Db_instance, "db-instance"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
