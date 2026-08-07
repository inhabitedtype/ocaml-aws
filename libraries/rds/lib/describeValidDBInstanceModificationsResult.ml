type t =
  { valid_d_b_instance_modifications_message :
      ValidDBInstanceModificationsMessage.t option
  }

let make ?valid_d_b_instance_modifications_message () =
  { valid_d_b_instance_modifications_message }

let parse xml =
  Some
    { valid_d_b_instance_modifications_message =
        Aws.Util.option_bind
          (Aws.Xml.member "ValidDBInstanceModificationsMessage" xml)
          ValidDBInstanceModificationsMessage.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.valid_d_b_instance_modifications_message (fun f ->
             Aws.Query.Pair
               ( "ValidDBInstanceModificationsMessage"
               , ValidDBInstanceModificationsMessage.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.valid_d_b_instance_modifications_message (fun f ->
             ( "ValidDBInstanceModificationsMessage"
             , ValidDBInstanceModificationsMessage.to_json f ))
       ])

let of_json j =
  { valid_d_b_instance_modifications_message =
      Aws.Util.option_map
        (Aws.Json.lookup j "ValidDBInstanceModificationsMessage")
        ValidDBInstanceModificationsMessage.of_json
  }
