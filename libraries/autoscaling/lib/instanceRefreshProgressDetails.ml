type t =
  { live_pool_progress : InstanceRefreshLivePoolProgress.t option
  ; warm_pool_progress : InstanceRefreshWarmPoolProgress.t option
  }

let make ?live_pool_progress ?warm_pool_progress () =
  { live_pool_progress; warm_pool_progress }

let parse xml =
  Some
    { live_pool_progress =
        Aws.Util.option_bind
          (Aws.Xml.member "LivePoolProgress" xml)
          InstanceRefreshLivePoolProgress.parse
    ; warm_pool_progress =
        Aws.Util.option_bind
          (Aws.Xml.member "WarmPoolProgress" xml)
          InstanceRefreshWarmPoolProgress.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warm_pool_progress (fun f ->
             Aws.Query.Pair
               ("WarmPoolProgress", InstanceRefreshWarmPoolProgress.to_query f))
       ; Aws.Util.option_map v.live_pool_progress (fun f ->
             Aws.Query.Pair
               ("LivePoolProgress", InstanceRefreshLivePoolProgress.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warm_pool_progress (fun f ->
             "WarmPoolProgress", InstanceRefreshWarmPoolProgress.to_json f)
       ; Aws.Util.option_map v.live_pool_progress (fun f ->
             "LivePoolProgress", InstanceRefreshLivePoolProgress.to_json f)
       ])

let of_json j =
  { live_pool_progress =
      Aws.Util.option_map
        (Aws.Json.lookup j "LivePoolProgress")
        InstanceRefreshLivePoolProgress.of_json
  ; warm_pool_progress =
      Aws.Util.option_map
        (Aws.Json.lookup j "WarmPoolProgress")
        InstanceRefreshWarmPoolProgress.of_json
  }
