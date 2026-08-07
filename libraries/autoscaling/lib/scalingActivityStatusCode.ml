open Aws.BaseTypes

type t =
  | PendingSpotBidPlacement
  | WaitingForSpotInstanceRequestId
  | WaitingForSpotInstanceId
  | WaitingForInstanceId
  | PreInService
  | InProgress
  | WaitingForELBConnectionDraining
  | MidLifecycleAction
  | WaitingForInstanceWarmup
  | Successful
  | Failed
  | Cancelled
  | WaitingForConnectionDraining
  | WaitingForInPlaceUpdateToStart
  | WaitingForInPlaceUpdateToFinalize
  | InPlaceUpdateInProgress

let str_to_t =
  [ "InPlaceUpdateInProgress", InPlaceUpdateInProgress
  ; "WaitingForInPlaceUpdateToFinalize", WaitingForInPlaceUpdateToFinalize
  ; "WaitingForInPlaceUpdateToStart", WaitingForInPlaceUpdateToStart
  ; "WaitingForConnectionDraining", WaitingForConnectionDraining
  ; "Cancelled", Cancelled
  ; "Failed", Failed
  ; "Successful", Successful
  ; "WaitingForInstanceWarmup", WaitingForInstanceWarmup
  ; "MidLifecycleAction", MidLifecycleAction
  ; "WaitingForELBConnectionDraining", WaitingForELBConnectionDraining
  ; "InProgress", InProgress
  ; "PreInService", PreInService
  ; "WaitingForInstanceId", WaitingForInstanceId
  ; "WaitingForSpotInstanceId", WaitingForSpotInstanceId
  ; "WaitingForSpotInstanceRequestId", WaitingForSpotInstanceRequestId
  ; "PendingSpotBidPlacement", PendingSpotBidPlacement
  ]

let t_to_str =
  [ InPlaceUpdateInProgress, "InPlaceUpdateInProgress"
  ; WaitingForInPlaceUpdateToFinalize, "WaitingForInPlaceUpdateToFinalize"
  ; WaitingForInPlaceUpdateToStart, "WaitingForInPlaceUpdateToStart"
  ; WaitingForConnectionDraining, "WaitingForConnectionDraining"
  ; Cancelled, "Cancelled"
  ; Failed, "Failed"
  ; Successful, "Successful"
  ; WaitingForInstanceWarmup, "WaitingForInstanceWarmup"
  ; MidLifecycleAction, "MidLifecycleAction"
  ; WaitingForELBConnectionDraining, "WaitingForELBConnectionDraining"
  ; InProgress, "InProgress"
  ; PreInService, "PreInService"
  ; WaitingForInstanceId, "WaitingForInstanceId"
  ; WaitingForSpotInstanceId, "WaitingForSpotInstanceId"
  ; WaitingForSpotInstanceRequestId, "WaitingForSpotInstanceRequestId"
  ; PendingSpotBidPlacement, "PendingSpotBidPlacement"
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
