type t = { instance_monitorings : InstanceMonitoringList.t }

let make ?(instance_monitorings = []) () = { instance_monitorings }

let parse xml =
  Some
    { instance_monitorings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instancesSet" xml)
             InstanceMonitoringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("InstancesSet", InstanceMonitoringList.to_query v.instance_monitorings))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("instancesSet", InstanceMonitoringList.to_json v.instance_monitorings) ])

let of_json j =
  { instance_monitorings =
      InstanceMonitoringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instancesSet"))
  }
