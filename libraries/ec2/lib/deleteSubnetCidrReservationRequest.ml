open Aws.BaseTypes

type t =
  { subnet_cidr_reservation_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~subnet_cidr_reservation_id ?dry_run () = { subnet_cidr_reservation_id; dry_run }

let parse xml =
  Some
    { subnet_cidr_reservation_id =
        Aws.Xml.required
          "SubnetCidrReservationId"
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetCidrReservationId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SubnetCidrReservationId", String.to_query v.subnet_cidr_reservation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("SubnetCidrReservationId", String.to_json v.subnet_cidr_reservation_id)
       ])

let of_json j =
  { subnet_cidr_reservation_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetCidrReservationId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
