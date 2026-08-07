open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; force_failover : Boolean.t option
  }

let make ~d_b_instance_identifier ?force_failover () =
  { d_b_instance_identifier; force_failover }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; force_failover =
        Aws.Util.option_bind (Aws.Xml.member "ForceFailover" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.force_failover (fun f ->
             Aws.Query.Pair ("ForceFailover", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.force_failover (fun f ->
             "ForceFailover", Boolean.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; force_failover =
      Aws.Util.option_map (Aws.Json.lookup j "ForceFailover") Boolean.of_json
  }
