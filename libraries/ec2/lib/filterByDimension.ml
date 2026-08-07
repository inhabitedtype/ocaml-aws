open Aws.BaseTypes

type t =
  | Resource_region
  | Availability_zone_id
  | Account_id
  | Account_name
  | Instance_family
  | Instance_type
  | Instance_platform
  | Reservation_arn
  | Reservation_id
  | Reservation_type
  | Reservation_create_timestamp
  | Reservation_start_timestamp
  | Reservation_end_timestamp
  | Reservation_end_date_type
  | Tenancy
  | Reservation_state
  | Reservation_instance_match_criteria
  | Reservation_unused_financial_owner

let str_to_t =
  [ "reservation-unused-financial-owner", Reservation_unused_financial_owner
  ; "reservation-instance-match-criteria", Reservation_instance_match_criteria
  ; "reservation-state", Reservation_state
  ; "tenancy", Tenancy
  ; "reservation-end-date-type", Reservation_end_date_type
  ; "reservation-end-timestamp", Reservation_end_timestamp
  ; "reservation-start-timestamp", Reservation_start_timestamp
  ; "reservation-create-timestamp", Reservation_create_timestamp
  ; "reservation-type", Reservation_type
  ; "reservation-id", Reservation_id
  ; "reservation-arn", Reservation_arn
  ; "instance-platform", Instance_platform
  ; "instance-type", Instance_type
  ; "instance-family", Instance_family
  ; "account-name", Account_name
  ; "account-id", Account_id
  ; "availability-zone-id", Availability_zone_id
  ; "resource-region", Resource_region
  ]

let t_to_str =
  [ Reservation_unused_financial_owner, "reservation-unused-financial-owner"
  ; Reservation_instance_match_criteria, "reservation-instance-match-criteria"
  ; Reservation_state, "reservation-state"
  ; Tenancy, "tenancy"
  ; Reservation_end_date_type, "reservation-end-date-type"
  ; Reservation_end_timestamp, "reservation-end-timestamp"
  ; Reservation_start_timestamp, "reservation-start-timestamp"
  ; Reservation_create_timestamp, "reservation-create-timestamp"
  ; Reservation_type, "reservation-type"
  ; Reservation_id, "reservation-id"
  ; Reservation_arn, "reservation-arn"
  ; Instance_platform, "instance-platform"
  ; Instance_type, "instance-type"
  ; Instance_family, "instance-family"
  ; Account_name, "account-name"
  ; Account_id, "account-id"
  ; Availability_zone_id, "availability-zone-id"
  ; Resource_region, "resource-region"
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
