open Aws.BaseTypes

type t =
  { marker : String.t option
  ; service_updates : ServiceUpdateList.t
  }

let make ?marker ?(service_updates = []) () = { marker; service_updates }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; service_updates =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ServiceUpdates" xml)
             ServiceUpdateList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ServiceUpdates.member", ServiceUpdateList.to_query v.service_updates))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ServiceUpdates", ServiceUpdateList.to_json v.service_updates)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; service_updates =
      ServiceUpdateList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdates"))
  }
