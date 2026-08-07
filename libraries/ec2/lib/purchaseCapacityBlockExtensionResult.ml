type t = { capacity_block_extensions : CapacityBlockExtensionSet.t }

let make ?(capacity_block_extensions = []) () = { capacity_block_extensions }

let parse xml =
  Some
    { capacity_block_extensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityBlockExtensionSet" xml)
             CapacityBlockExtensionSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityBlockExtensionSet"
              , CapacityBlockExtensionSet.to_query v.capacity_block_extensions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "capacityBlockExtensionSet"
           , CapacityBlockExtensionSet.to_json v.capacity_block_extensions )
       ])

let of_json j =
  { capacity_block_extensions =
      CapacityBlockExtensionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityBlockExtensionSet"))
  }
