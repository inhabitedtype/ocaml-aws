(** This contains per service mapping to endpoint URLs.
    An endpoint is the URL of the entry point for an AWS web service.

    See {{:https://docs.aws.amazon.com/general/latest/gr/rande.html} [AWS Service Endpoints]} documentation
    for further details. *)

module Endpoints : sig

  (** Optionally produce an endpoint from the [service] and [region]. *)
  val endpoint_of : string -> string -> string option

  (** Optionally produce an endpoint URL from the [service] and [region]. *)
  val url_of : string -> string -> string option
end
