open Aws.BaseTypes

type t =
  { orderable_d_b_instance_options : OrderableDBInstanceOptionsList.t
  ; marker : String.t option
  }

let make ?(orderable_d_b_instance_options = []) ?marker () =
  { orderable_d_b_instance_options; marker }

let parse xml =
  Some
    { orderable_d_b_instance_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OrderableDBInstanceOptions" xml)
             OrderableDBInstanceOptionsList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OrderableDBInstanceOptions.member"
              , OrderableDBInstanceOptionsList.to_query v.orderable_d_b_instance_options
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some
           ( "OrderableDBInstanceOptions"
           , OrderableDBInstanceOptionsList.to_json v.orderable_d_b_instance_options )
       ])

let of_json j =
  { orderable_d_b_instance_options =
      OrderableDBInstanceOptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OrderableDBInstanceOptions"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
