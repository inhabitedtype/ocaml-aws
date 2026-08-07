open Aws.BaseTypes

type t =
  { bucket : String.t option
  ; fault : SpotInstanceStateFault.t option
  ; owner_id : String.t option
  ; prefix : String.t option
  ; state : DatafeedSubscriptionState.t option
  }

let make ?bucket ?fault ?owner_id ?prefix ?state () =
  { bucket; fault; owner_id; prefix; state }

let parse xml =
  Some
    { bucket = Aws.Util.option_bind (Aws.Xml.member "bucket" xml) String.parse
    ; fault =
        Aws.Util.option_bind (Aws.Xml.member "fault" xml) SpotInstanceStateFault.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; prefix = Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) DatafeedSubscriptionState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", DatafeedSubscriptionState.to_query f))
       ; Aws.Util.option_map v.prefix (fun f ->
             Aws.Query.Pair ("Prefix", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.fault (fun f ->
             Aws.Query.Pair ("Fault", SpotInstanceStateFault.to_query f))
       ; Aws.Util.option_map v.bucket (fun f ->
             Aws.Query.Pair ("Bucket", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", DatafeedSubscriptionState.to_json f)
       ; Aws.Util.option_map v.prefix (fun f -> "prefix", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.fault (fun f -> "fault", SpotInstanceStateFault.to_json f)
       ; Aws.Util.option_map v.bucket (fun f -> "bucket", String.to_json f)
       ])

let of_json j =
  { bucket = Aws.Util.option_map (Aws.Json.lookup j "bucket") String.of_json
  ; fault = Aws.Util.option_map (Aws.Json.lookup j "fault") SpotInstanceStateFault.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; prefix = Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") DatafeedSubscriptionState.of_json
  }
