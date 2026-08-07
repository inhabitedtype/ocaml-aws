open Aws.BaseTypes

type t =
  | Reservation_total_capacity_hrs_vcpu
  | Reservation_total_capacity_hrs_inst
  | Reservation_max_size_vcpu
  | Reservation_max_size_inst
  | Reservation_min_size_vcpu
  | Reservation_min_size_inst
  | Reservation_unused_total_capacity_hrs_vcpu
  | Reservation_unused_total_capacity_hrs_inst
  | Reservation_unused_total_estimated_cost
  | Reservation_max_unused_size_vcpu
  | Reservation_max_unused_size_inst
  | Reservation_min_unused_size_vcpu
  | Reservation_min_unused_size_inst
  | Reservation_max_utilization
  | Reservation_min_utilization
  | Reservation_avg_utilization_vcpu
  | Reservation_avg_utilization_inst
  | Reservation_total_count
  | Reservation_total_estimated_cost
  | Reservation_avg_future_size_vcpu
  | Reservation_avg_future_size_inst
  | Reservation_min_future_size_vcpu
  | Reservation_min_future_size_inst
  | Reservation_max_future_size_vcpu
  | Reservation_max_future_size_inst
  | Reservation_avg_committed_size_vcpu
  | Reservation_avg_committed_size_inst
  | Reservation_max_committed_size_vcpu
  | Reservation_max_committed_size_inst
  | Reservation_min_committed_size_vcpu
  | Reservation_min_committed_size_inst
  | Reserved_total_usage_hrs_vcpu
  | Reserved_total_usage_hrs_inst
  | Reserved_total_estimated_cost
  | Unreserved_total_usage_hrs_vcpu
  | Unreserved_total_usage_hrs_inst
  | Unreserved_total_estimated_cost
  | Spot_total_usage_hrs_vcpu
  | Spot_total_usage_hrs_inst
  | Spot_total_estimated_cost
  | Spot_avg_run_time_before_interruption_inst
  | Spot_max_run_time_before_interruption_inst
  | Spot_min_run_time_before_interruption_inst
  | Spot_total_interruptions_inst
  | Spot_total_interruptions_vcpu
  | Spot_total_count_inst
  | Spot_total_count_vcpu
  | Spot_interruption_rate_inst
  | Spot_interruption_rate_vcpu

let str_to_t =
  [ "spot-interruption-rate-vcpu", Spot_interruption_rate_vcpu
  ; "spot-interruption-rate-inst", Spot_interruption_rate_inst
  ; "spot-total-count-vcpu", Spot_total_count_vcpu
  ; "spot-total-count-inst", Spot_total_count_inst
  ; "spot-total-interruptions-vcpu", Spot_total_interruptions_vcpu
  ; "spot-total-interruptions-inst", Spot_total_interruptions_inst
  ; ( "spot-min-run-time-before-interruption-inst"
    , Spot_min_run_time_before_interruption_inst )
  ; ( "spot-max-run-time-before-interruption-inst"
    , Spot_max_run_time_before_interruption_inst )
  ; ( "spot-avg-run-time-before-interruption-inst"
    , Spot_avg_run_time_before_interruption_inst )
  ; "spot-total-estimated-cost", Spot_total_estimated_cost
  ; "spot-total-usage-hrs-inst", Spot_total_usage_hrs_inst
  ; "spot-total-usage-hrs-vcpu", Spot_total_usage_hrs_vcpu
  ; "unreserved-total-estimated-cost", Unreserved_total_estimated_cost
  ; "unreserved-total-usage-hrs-inst", Unreserved_total_usage_hrs_inst
  ; "unreserved-total-usage-hrs-vcpu", Unreserved_total_usage_hrs_vcpu
  ; "reserved-total-estimated-cost", Reserved_total_estimated_cost
  ; "reserved-total-usage-hrs-inst", Reserved_total_usage_hrs_inst
  ; "reserved-total-usage-hrs-vcpu", Reserved_total_usage_hrs_vcpu
  ; "reservation-min-committed-size-inst", Reservation_min_committed_size_inst
  ; "reservation-min-committed-size-vcpu", Reservation_min_committed_size_vcpu
  ; "reservation-max-committed-size-inst", Reservation_max_committed_size_inst
  ; "reservation-max-committed-size-vcpu", Reservation_max_committed_size_vcpu
  ; "reservation-avg-committed-size-inst", Reservation_avg_committed_size_inst
  ; "reservation-avg-committed-size-vcpu", Reservation_avg_committed_size_vcpu
  ; "reservation-max-future-size-inst", Reservation_max_future_size_inst
  ; "reservation-max-future-size-vcpu", Reservation_max_future_size_vcpu
  ; "reservation-min-future-size-inst", Reservation_min_future_size_inst
  ; "reservation-min-future-size-vcpu", Reservation_min_future_size_vcpu
  ; "reservation-avg-future-size-inst", Reservation_avg_future_size_inst
  ; "reservation-avg-future-size-vcpu", Reservation_avg_future_size_vcpu
  ; "reservation-total-estimated-cost", Reservation_total_estimated_cost
  ; "reservation-total-count", Reservation_total_count
  ; "reservation-avg-utilization-inst", Reservation_avg_utilization_inst
  ; "reservation-avg-utilization-vcpu", Reservation_avg_utilization_vcpu
  ; "reservation-min-utilization", Reservation_min_utilization
  ; "reservation-max-utilization", Reservation_max_utilization
  ; "reservation-min-unused-size-inst", Reservation_min_unused_size_inst
  ; "reservation-min-unused-size-vcpu", Reservation_min_unused_size_vcpu
  ; "reservation-max-unused-size-inst", Reservation_max_unused_size_inst
  ; "reservation-max-unused-size-vcpu", Reservation_max_unused_size_vcpu
  ; "reservation-unused-total-estimated-cost", Reservation_unused_total_estimated_cost
  ; ( "reservation-unused-total-capacity-hrs-inst"
    , Reservation_unused_total_capacity_hrs_inst )
  ; ( "reservation-unused-total-capacity-hrs-vcpu"
    , Reservation_unused_total_capacity_hrs_vcpu )
  ; "reservation-min-size-inst", Reservation_min_size_inst
  ; "reservation-min-size-vcpu", Reservation_min_size_vcpu
  ; "reservation-max-size-inst", Reservation_max_size_inst
  ; "reservation-max-size-vcpu", Reservation_max_size_vcpu
  ; "reservation-total-capacity-hrs-inst", Reservation_total_capacity_hrs_inst
  ; "reservation-total-capacity-hrs-vcpu", Reservation_total_capacity_hrs_vcpu
  ]

let t_to_str =
  [ Spot_interruption_rate_vcpu, "spot-interruption-rate-vcpu"
  ; Spot_interruption_rate_inst, "spot-interruption-rate-inst"
  ; Spot_total_count_vcpu, "spot-total-count-vcpu"
  ; Spot_total_count_inst, "spot-total-count-inst"
  ; Spot_total_interruptions_vcpu, "spot-total-interruptions-vcpu"
  ; Spot_total_interruptions_inst, "spot-total-interruptions-inst"
  ; ( Spot_min_run_time_before_interruption_inst
    , "spot-min-run-time-before-interruption-inst" )
  ; ( Spot_max_run_time_before_interruption_inst
    , "spot-max-run-time-before-interruption-inst" )
  ; ( Spot_avg_run_time_before_interruption_inst
    , "spot-avg-run-time-before-interruption-inst" )
  ; Spot_total_estimated_cost, "spot-total-estimated-cost"
  ; Spot_total_usage_hrs_inst, "spot-total-usage-hrs-inst"
  ; Spot_total_usage_hrs_vcpu, "spot-total-usage-hrs-vcpu"
  ; Unreserved_total_estimated_cost, "unreserved-total-estimated-cost"
  ; Unreserved_total_usage_hrs_inst, "unreserved-total-usage-hrs-inst"
  ; Unreserved_total_usage_hrs_vcpu, "unreserved-total-usage-hrs-vcpu"
  ; Reserved_total_estimated_cost, "reserved-total-estimated-cost"
  ; Reserved_total_usage_hrs_inst, "reserved-total-usage-hrs-inst"
  ; Reserved_total_usage_hrs_vcpu, "reserved-total-usage-hrs-vcpu"
  ; Reservation_min_committed_size_inst, "reservation-min-committed-size-inst"
  ; Reservation_min_committed_size_vcpu, "reservation-min-committed-size-vcpu"
  ; Reservation_max_committed_size_inst, "reservation-max-committed-size-inst"
  ; Reservation_max_committed_size_vcpu, "reservation-max-committed-size-vcpu"
  ; Reservation_avg_committed_size_inst, "reservation-avg-committed-size-inst"
  ; Reservation_avg_committed_size_vcpu, "reservation-avg-committed-size-vcpu"
  ; Reservation_max_future_size_inst, "reservation-max-future-size-inst"
  ; Reservation_max_future_size_vcpu, "reservation-max-future-size-vcpu"
  ; Reservation_min_future_size_inst, "reservation-min-future-size-inst"
  ; Reservation_min_future_size_vcpu, "reservation-min-future-size-vcpu"
  ; Reservation_avg_future_size_inst, "reservation-avg-future-size-inst"
  ; Reservation_avg_future_size_vcpu, "reservation-avg-future-size-vcpu"
  ; Reservation_total_estimated_cost, "reservation-total-estimated-cost"
  ; Reservation_total_count, "reservation-total-count"
  ; Reservation_avg_utilization_inst, "reservation-avg-utilization-inst"
  ; Reservation_avg_utilization_vcpu, "reservation-avg-utilization-vcpu"
  ; Reservation_min_utilization, "reservation-min-utilization"
  ; Reservation_max_utilization, "reservation-max-utilization"
  ; Reservation_min_unused_size_inst, "reservation-min-unused-size-inst"
  ; Reservation_min_unused_size_vcpu, "reservation-min-unused-size-vcpu"
  ; Reservation_max_unused_size_inst, "reservation-max-unused-size-inst"
  ; Reservation_max_unused_size_vcpu, "reservation-max-unused-size-vcpu"
  ; Reservation_unused_total_estimated_cost, "reservation-unused-total-estimated-cost"
  ; ( Reservation_unused_total_capacity_hrs_inst
    , "reservation-unused-total-capacity-hrs-inst" )
  ; ( Reservation_unused_total_capacity_hrs_vcpu
    , "reservation-unused-total-capacity-hrs-vcpu" )
  ; Reservation_min_size_inst, "reservation-min-size-inst"
  ; Reservation_min_size_vcpu, "reservation-min-size-vcpu"
  ; Reservation_max_size_inst, "reservation-max-size-inst"
  ; Reservation_max_size_vcpu, "reservation-max-size-vcpu"
  ; Reservation_total_capacity_hrs_inst, "reservation-total-capacity-hrs-inst"
  ; Reservation_total_capacity_hrs_vcpu, "reservation-total-capacity-hrs-vcpu"
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
