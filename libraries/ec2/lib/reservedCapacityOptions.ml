type t = { reservation_types : ReservationTypeList.t }

let make ?(reservation_types = []) () = { reservation_types }

let parse xml =
  Some
    { reservation_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reservationTypeSet" xml)
             ReservationTypeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ReservationTypeSet", ReservationTypeList.to_query v.reservation_types))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("reservationTypeSet", ReservationTypeList.to_json v.reservation_types) ])

let of_json j =
  { reservation_types =
      ReservationTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservationTypeSet"))
  }
