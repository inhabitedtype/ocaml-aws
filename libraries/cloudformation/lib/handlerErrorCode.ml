open Aws.BaseTypes

type t =
  | NotUpdatable
  | InvalidRequest
  | AccessDenied
  | InvalidCredentials
  | AlreadyExists
  | NotFound
  | ResourceConflict
  | Throttling
  | ServiceLimitExceeded
  | NotStabilized
  | GeneralServiceException
  | ServiceInternalError
  | NetworkFailure
  | InternalFailure
  | InvalidTypeConfiguration
  | HandlerInternalFailure
  | NonCompliant
  | Unknown
  | UnsupportedTarget

let str_to_t =
  [ "UnsupportedTarget", UnsupportedTarget
  ; "Unknown", Unknown
  ; "NonCompliant", NonCompliant
  ; "HandlerInternalFailure", HandlerInternalFailure
  ; "InvalidTypeConfiguration", InvalidTypeConfiguration
  ; "InternalFailure", InternalFailure
  ; "NetworkFailure", NetworkFailure
  ; "ServiceInternalError", ServiceInternalError
  ; "GeneralServiceException", GeneralServiceException
  ; "NotStabilized", NotStabilized
  ; "ServiceLimitExceeded", ServiceLimitExceeded
  ; "Throttling", Throttling
  ; "ResourceConflict", ResourceConflict
  ; "NotFound", NotFound
  ; "AlreadyExists", AlreadyExists
  ; "InvalidCredentials", InvalidCredentials
  ; "AccessDenied", AccessDenied
  ; "InvalidRequest", InvalidRequest
  ; "NotUpdatable", NotUpdatable
  ]

let t_to_str =
  [ UnsupportedTarget, "UnsupportedTarget"
  ; Unknown, "Unknown"
  ; NonCompliant, "NonCompliant"
  ; HandlerInternalFailure, "HandlerInternalFailure"
  ; InvalidTypeConfiguration, "InvalidTypeConfiguration"
  ; InternalFailure, "InternalFailure"
  ; NetworkFailure, "NetworkFailure"
  ; ServiceInternalError, "ServiceInternalError"
  ; GeneralServiceException, "GeneralServiceException"
  ; NotStabilized, "NotStabilized"
  ; ServiceLimitExceeded, "ServiceLimitExceeded"
  ; Throttling, "Throttling"
  ; ResourceConflict, "ResourceConflict"
  ; NotFound, "NotFound"
  ; AlreadyExists, "AlreadyExists"
  ; InvalidCredentials, "InvalidCredentials"
  ; AccessDenied, "AccessDenied"
  ; InvalidRequest, "InvalidRequest"
  ; NotUpdatable, "NotUpdatable"
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
