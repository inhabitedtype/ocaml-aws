open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; instance_metadata_options : InstanceMetadataOptionsResponse.t option
  }

let make ?instance_id ?instance_metadata_options () =
  { instance_id; instance_metadata_options }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_metadata_options =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMetadataOptions" xml)
          InstanceMetadataOptionsResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_options (fun f ->
             Aws.Query.Pair
               ("InstanceMetadataOptions", InstanceMetadataOptionsResponse.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_options (fun f ->
             "instanceMetadataOptions", InstanceMetadataOptionsResponse.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_metadata_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMetadataOptions")
        InstanceMetadataOptionsResponse.of_json
  }
