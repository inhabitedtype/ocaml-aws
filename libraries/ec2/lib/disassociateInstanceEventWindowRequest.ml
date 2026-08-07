open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_event_window_id : String.t
  ; association_target : InstanceEventWindowDisassociationRequest.t
  }

let make ?dry_run ~instance_event_window_id ~association_target () =
  { dry_run; instance_event_window_id; association_target }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_event_window_id =
        Aws.Xml.required
          "InstanceEventWindowId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceEventWindowId" xml) String.parse)
    ; association_target =
        Aws.Xml.required
          "AssociationTarget"
          (Aws.Util.option_bind
             (Aws.Xml.member "AssociationTarget" xml)
             InstanceEventWindowDisassociationRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AssociationTarget"
              , InstanceEventWindowDisassociationRequest.to_query v.association_target ))
       ; Some
           (Aws.Query.Pair
              ("InstanceEventWindowId", String.to_query v.instance_event_window_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AssociationTarget"
           , InstanceEventWindowDisassociationRequest.to_json v.association_target )
       ; Some ("InstanceEventWindowId", String.to_json v.instance_event_window_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_event_window_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceEventWindowId"))
  ; association_target =
      InstanceEventWindowDisassociationRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociationTarget"))
  }
