type t = { conversion_tasks : DescribeConversionTaskList.t }

let make ?(conversion_tasks = []) () = { conversion_tasks }

let parse xml =
  Some
    { conversion_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "conversionTasks" xml)
             DescribeConversionTaskList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ConversionTasks", DescribeConversionTaskList.to_query v.conversion_tasks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("conversionTasks", DescribeConversionTaskList.to_json v.conversion_tasks) ])

let of_json j =
  { conversion_tasks =
      DescribeConversionTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "conversionTasks"))
  }
