open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; bundle_id : String.t
  ; state : BundleTaskState.t
  ; start_time : DateTime.t
  ; update_time : DateTime.t
  ; storage : Storage.t
  ; progress : String.t
  ; bundle_task_error : BundleTaskError.t option
  }

let make
    ~instance_id
    ~bundle_id
    ~state
    ~start_time
    ~update_time
    ~storage
    ~progress
    ?bundle_task_error
    () =
  { instance_id
  ; bundle_id
  ; state
  ; start_time
  ; update_time
  ; storage
  ; progress
  ; bundle_task_error
  }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; bundle_id =
        Aws.Xml.required
          "bundleId"
          (Aws.Util.option_bind (Aws.Xml.member "bundleId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) BundleTaskState.parse)
    ; start_time =
        Aws.Xml.required
          "startTime"
          (Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse)
    ; update_time =
        Aws.Xml.required
          "updateTime"
          (Aws.Util.option_bind (Aws.Xml.member "updateTime" xml) DateTime.parse)
    ; storage =
        Aws.Xml.required
          "storage"
          (Aws.Util.option_bind (Aws.Xml.member "storage" xml) Storage.parse)
    ; progress =
        Aws.Xml.required
          "progress"
          (Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse)
    ; bundle_task_error =
        Aws.Util.option_bind (Aws.Xml.member "error" xml) BundleTaskError.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bundle_task_error (fun f ->
             Aws.Query.Pair ("Error", BundleTaskError.to_query f))
       ; Some (Aws.Query.Pair ("Progress", String.to_query v.progress))
       ; Some (Aws.Query.Pair ("Storage", Storage.to_query v.storage))
       ; Some (Aws.Query.Pair ("UpdateTime", DateTime.to_query v.update_time))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Some (Aws.Query.Pair ("State", BundleTaskState.to_query v.state))
       ; Some (Aws.Query.Pair ("BundleId", String.to_query v.bundle_id))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bundle_task_error (fun f ->
             "error", BundleTaskError.to_json f)
       ; Some ("progress", String.to_json v.progress)
       ; Some ("storage", Storage.to_json v.storage)
       ; Some ("updateTime", DateTime.to_json v.update_time)
       ; Some ("startTime", DateTime.to_json v.start_time)
       ; Some ("state", BundleTaskState.to_json v.state)
       ; Some ("bundleId", String.to_json v.bundle_id)
       ; Some ("instanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; bundle_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "bundleId"))
  ; state = BundleTaskState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "startTime"))
  ; update_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "updateTime"))
  ; storage = Storage.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "storage"))
  ; progress = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "progress"))
  ; bundle_task_error =
      Aws.Util.option_map (Aws.Json.lookup j "error") BundleTaskError.of_json
  }
