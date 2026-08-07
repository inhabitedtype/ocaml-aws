open Aws.BaseTypes

type t =
  { instance_image_metadata : InstanceImageMetadataList.t
  ; next_token : String.t option
  }

let make ?(instance_image_metadata = []) ?next_token () =
  { instance_image_metadata; next_token }

let parse xml =
  Some
    { instance_image_metadata =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceImageMetadataSet" xml)
             InstanceImageMetadataList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceImageMetadataSet"
              , InstanceImageMetadataList.to_query v.instance_image_metadata ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "instanceImageMetadataSet"
           , InstanceImageMetadataList.to_json v.instance_image_metadata )
       ])

let of_json j =
  { instance_image_metadata =
      InstanceImageMetadataList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceImageMetadataSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
