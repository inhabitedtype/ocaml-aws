open Aws.BaseTypes

type t =
  | MAX_HEALTH_CHECKS_BY_OWNER
  | MAX_HOSTED_ZONES_BY_OWNER
  | MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER
  | MAX_REUSABLE_DELEGATION_SETS_BY_OWNER
  | MAX_TRAFFIC_POLICIES_BY_OWNER

let str_to_t =
  [ "MAX_TRAFFIC_POLICIES_BY_OWNER", MAX_TRAFFIC_POLICIES_BY_OWNER
  ; "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER", MAX_REUSABLE_DELEGATION_SETS_BY_OWNER
  ; "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER", MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER
  ; "MAX_HOSTED_ZONES_BY_OWNER", MAX_HOSTED_ZONES_BY_OWNER
  ; "MAX_HEALTH_CHECKS_BY_OWNER", MAX_HEALTH_CHECKS_BY_OWNER
  ]

let t_to_str =
  [ MAX_TRAFFIC_POLICIES_BY_OWNER, "MAX_TRAFFIC_POLICIES_BY_OWNER"
  ; MAX_REUSABLE_DELEGATION_SETS_BY_OWNER, "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"
  ; MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER, "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"
  ; MAX_HOSTED_ZONES_BY_OWNER, "MAX_HOSTED_ZONES_BY_OWNER"
  ; MAX_HEALTH_CHECKS_BY_OWNER, "MAX_HEALTH_CHECKS_BY_OWNER"
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
