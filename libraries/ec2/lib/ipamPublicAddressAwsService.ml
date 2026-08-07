open Aws.BaseTypes

type t =
  | Nat_gateway
  | Database_migration_service
  | Redshift
  | Elastic_container_service
  | Relational_database_service
  | Site_to_site_vpn
  | Load_balancer
  | Global_accelerator
  | Cloudfront
  | Other

let str_to_t =
  [ "other", Other
  ; "cloudfront", Cloudfront
  ; "global-accelerator", Global_accelerator
  ; "load-balancer", Load_balancer
  ; "site-to-site-vpn", Site_to_site_vpn
  ; "relational-database-service", Relational_database_service
  ; "elastic-container-service", Elastic_container_service
  ; "redshift", Redshift
  ; "database-migration-service", Database_migration_service
  ; "nat-gateway", Nat_gateway
  ]

let t_to_str =
  [ Other, "other"
  ; Cloudfront, "cloudfront"
  ; Global_accelerator, "global-accelerator"
  ; Load_balancer, "load-balancer"
  ; Site_to_site_vpn, "site-to-site-vpn"
  ; Relational_database_service, "relational-database-service"
  ; Elastic_container_service, "elastic-container-service"
  ; Redshift, "redshift"
  ; Database_migration_service, "database-migration-service"
  ; Nat_gateway, "nat-gateway"
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
