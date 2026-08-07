open Aws.BaseTypes

type t =
  { subnet_id : String.t
  ; cidr : String.t
  ; reservation_type : SubnetCidrReservationType.t
  ; description : String.t option
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~subnet_id
    ~cidr
    ~reservation_type
    ?description
    ?dry_run
    ?(tag_specifications = [])
    () =
  { subnet_id; cidr; reservation_type; description; dry_run; tag_specifications }

let parse xml =
  Some
    { subnet_id =
        Aws.Xml.required
          "SubnetId"
          (Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse)
    ; cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    ; reservation_type =
        Aws.Xml.required
          "ReservationType"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservationType" xml)
             SubnetCidrReservationType.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ReservationType", SubnetCidrReservationType.to_query v.reservation_type))
       ; Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("ReservationType", SubnetCidrReservationType.to_json v.reservation_type)
       ; Some ("Cidr", String.to_json v.cidr)
       ; Some ("SubnetId", String.to_json v.subnet_id)
       ])

let of_json j =
  { subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  ; reservation_type =
      SubnetCidrReservationType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservationType"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
