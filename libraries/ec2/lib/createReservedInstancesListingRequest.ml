open Aws.BaseTypes

type t =
  { reserved_instances_id : String.t
  ; instance_count : Integer.t
  ; price_schedules : PriceScheduleSpecificationList.t
  ; client_token : String.t
  }

let make ~reserved_instances_id ~instance_count ~price_schedules ~client_token () =
  { reserved_instances_id; instance_count; price_schedules; client_token }

let parse xml =
  Some
    { reserved_instances_id =
        Aws.Xml.required
          "reservedInstancesId"
          (Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse)
    ; instance_count =
        Aws.Xml.required
          "instanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse)
    ; price_schedules =
        Aws.Xml.required
          "priceSchedules"
          (Aws.Util.option_bind
             (Aws.Xml.member "priceSchedules" xml)
             PriceScheduleSpecificationList.parse)
    ; client_token =
        Aws.Xml.required
          "clientToken"
          (Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ClientToken", String.to_query v.client_token))
       ; Some
           (Aws.Query.Pair
              ("PriceSchedules", PriceScheduleSpecificationList.to_query v.price_schedules))
       ; Some (Aws.Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
       ; Some
           (Aws.Query.Pair ("ReservedInstancesId", String.to_query v.reserved_instances_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("clientToken", String.to_json v.client_token)
       ; Some ("priceSchedules", PriceScheduleSpecificationList.to_json v.price_schedules)
       ; Some ("instanceCount", Integer.to_json v.instance_count)
       ; Some ("reservedInstancesId", String.to_json v.reserved_instances_id)
       ])

let of_json j =
  { reserved_instances_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstancesId"))
  ; instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceCount"))
  ; price_schedules =
      PriceScheduleSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "priceSchedules"))
  ; client_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "clientToken"))
  }
