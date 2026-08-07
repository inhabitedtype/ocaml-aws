open Aws.BaseTypes

type t =
  | Cache_cluster
  | Cache_parameter_group
  | Cache_security_group
  | Cache_subnet_group
  | Replication_group
  | Serverless_cache
  | Serverless_cache_snapshot
  | User
  | User_group

let str_to_t =
  [ "user-group", User_group
  ; "user", User
  ; "serverless-cache-snapshot", Serverless_cache_snapshot
  ; "serverless-cache", Serverless_cache
  ; "replication-group", Replication_group
  ; "cache-subnet-group", Cache_subnet_group
  ; "cache-security-group", Cache_security_group
  ; "cache-parameter-group", Cache_parameter_group
  ; "cache-cluster", Cache_cluster
  ]

let t_to_str =
  [ User_group, "user-group"
  ; User, "user"
  ; Serverless_cache_snapshot, "serverless-cache-snapshot"
  ; Serverless_cache, "serverless-cache"
  ; Replication_group, "replication-group"
  ; Cache_subnet_group, "cache-subnet-group"
  ; Cache_security_group, "cache-security-group"
  ; Cache_parameter_group, "cache-parameter-group"
  ; Cache_cluster, "cache-cluster"
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
