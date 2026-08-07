open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; timestamp : DateTime.t
  ; password_data : String.t
  }

let make ~instance_id ~timestamp ~password_data () =
  { instance_id; timestamp; password_data }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; timestamp =
        Aws.Xml.required
          "timestamp"
          (Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) DateTime.parse)
    ; password_data =
        Aws.Xml.required
          "passwordData"
          (Aws.Util.option_bind (Aws.Xml.member "passwordData" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PasswordData", String.to_query v.password_data))
       ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("passwordData", String.to_json v.password_data)
       ; Some ("timestamp", DateTime.to_json v.timestamp)
       ; Some ("instanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "timestamp"))
  ; password_data =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "passwordData"))
  }
