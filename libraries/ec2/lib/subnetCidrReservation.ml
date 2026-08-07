open Aws.BaseTypes

type t =
  { subnet_cidr_reservation_id : String.t option
  ; subnet_id : String.t option
  ; cidr : String.t option
  ; reservation_type : SubnetCidrReservationType.t option
  ; owner_id : String.t option
  ; description : String.t option
  ; tags : TagList.t
  }

let make
    ?subnet_cidr_reservation_id
    ?subnet_id
    ?cidr
    ?reservation_type
    ?owner_id
    ?description
    ?(tags = [])
    () =
  { subnet_cidr_reservation_id
  ; subnet_id
  ; cidr
  ; reservation_type
  ; owner_id
  ; description
  ; tags
  }

let parse xml =
  Some
    { subnet_cidr_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "subnetCidrReservationId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; reservation_type =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationType" xml)
          SubnetCidrReservationType.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.reservation_type (fun f ->
             Aws.Query.Pair ("ReservationType", SubnetCidrReservationType.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.subnet_cidr_reservation_id (fun f ->
             Aws.Query.Pair ("SubnetCidrReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.reservation_type (fun f ->
             "reservationType", SubnetCidrReservationType.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.subnet_cidr_reservation_id (fun f ->
             "subnetCidrReservationId", String.to_json f)
       ])

let of_json j =
  { subnet_cidr_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "subnetCidrReservationId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; reservation_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservationType")
        SubnetCidrReservationType.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
