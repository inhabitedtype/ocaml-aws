type t =
  { successful_instance_credit_specifications : SuccessfulInstanceCreditSpecificationSet.t
  ; unsuccessful_instance_credit_specifications :
      UnsuccessfulInstanceCreditSpecificationSet.t
  }

let make
    ?(successful_instance_credit_specifications = [])
    ?(unsuccessful_instance_credit_specifications = [])
    () =
  { successful_instance_credit_specifications
  ; unsuccessful_instance_credit_specifications
  }

let parse xml =
  Some
    { successful_instance_credit_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successfulInstanceCreditSpecificationSet" xml)
             SuccessfulInstanceCreditSpecificationSet.parse)
    ; unsuccessful_instance_credit_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessfulInstanceCreditSpecificationSet" xml)
             UnsuccessfulInstanceCreditSpecificationSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnsuccessfulInstanceCreditSpecificationSet"
              , UnsuccessfulInstanceCreditSpecificationSet.to_query
                  v.unsuccessful_instance_credit_specifications ))
       ; Some
           (Aws.Query.Pair
              ( "SuccessfulInstanceCreditSpecificationSet"
              , SuccessfulInstanceCreditSpecificationSet.to_query
                  v.successful_instance_credit_specifications ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "unsuccessfulInstanceCreditSpecificationSet"
           , UnsuccessfulInstanceCreditSpecificationSet.to_json
               v.unsuccessful_instance_credit_specifications )
       ; Some
           ( "successfulInstanceCreditSpecificationSet"
           , SuccessfulInstanceCreditSpecificationSet.to_json
               v.successful_instance_credit_specifications )
       ])

let of_json j =
  { successful_instance_credit_specifications =
      SuccessfulInstanceCreditSpecificationSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "successfulInstanceCreditSpecificationSet"))
  ; unsuccessful_instance_credit_specifications =
      UnsuccessfulInstanceCreditSpecificationSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "unsuccessfulInstanceCreditSpecificationSet"))
  }
