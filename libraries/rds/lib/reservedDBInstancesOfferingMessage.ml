open Aws.BaseTypes

type t =
  { marker : String.t option
  ; reserved_d_b_instances_offerings : ReservedDBInstancesOfferingList.t
  }

let make ?marker ?(reserved_d_b_instances_offerings = []) () =
  { marker; reserved_d_b_instances_offerings }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; reserved_d_b_instances_offerings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedDBInstancesOfferings" xml)
             ReservedDBInstancesOfferingList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedDBInstancesOfferings.member"
              , ReservedDBInstancesOfferingList.to_query
                  v.reserved_d_b_instances_offerings ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ReservedDBInstancesOfferings"
           , ReservedDBInstancesOfferingList.to_json v.reserved_d_b_instances_offerings )
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; reserved_d_b_instances_offerings =
      ReservedDBInstancesOfferingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedDBInstancesOfferings"))
  }
