(* open OUnit
 * open Aws_s3
 *
 * module TestSuite(Runtime : sig
 *     type +'a m
 *     val run_request :
 *         (module Aws.Call with type input = 'input
 *                            and type output = 'output
 *                            and type error = 'error)
 *       -> 'input
 *       -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
 *     val un_m : 'a m -> 'a
 *   end) = struct
 *
 *   let create_bucket bucket =
 *     Runtime.(un_m (run_request (module CreateBucket)
 *                      (Types.CreateBucketRequest.make ~bucket ())))
 *
 *   let delete_bucket bucket =
 *     Runtime.(un_m (run_request (module DeleteBucket)
 *                      (Types.DeleteBucketRequest.make ~bucket ())))
 *
 *   let list_buckets =
 *     Runtime.(un_m (run_request (module ListBuckets) ()))
 *
 *   let arb_bucket = QCheck.Gen.oneofl ["test-bucket"]
 *
 *   (\* let list_bucket_test =
 *    *   QCheck.Test.make ~count:1 ~max_fail:1
 *    *     ~name:"s3 create and delete bucket"
 *    *     QCheck.(QCheck.make arb_bucket)
 *    *     (fun bucket_name ->
 *    *       match list_buckets with
 *    *       | `Ok resp ->
 *    *          Printf.printf "%s\n" (Yojson.Basic.to_string (Types.ListBucketsOutput.(to_json (of_json (to_json resp)))));
 *    *          true
 *    *       | `Error err ->
 *    *          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
 *    *          false *\)
 *       (\* ) *\)
 *
 *   let create_delete_bucket_test =
 *     QCheck.Test.make ~count:1
 *       ~name:"S3 create and delete bucket"
 *       QCheck.(QCheck.make arb_bucket)
 *       (fun bucket_name ->
 *         let create_res = create_bucket bucket_name in
 *
 *         match create_res with
 *         | `Ok resp ->
 *            Printf.printf "%s\n" (Yojson.Basic.to_string (Types.CreateBucketOutput.(to_json (of_json (to_json resp)))));
 *         | `Error err ->
 *            Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
 *            QCheck.assume_fail ();
 *            ;
 *        let delete_res = delete_bucket bucket_name in
 *        match delete_res with
 *        | `Ok resp ->
 *           true
 *        | `Error err ->
 *           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
 *           false
 *       )
 *
 *   let test_cases =
 *     [ create_delete_bucket_test]
 *
 *   let rec was_successful =
 *     function
 *     | [] -> true
 *     | RSuccess _::t
 *     | RSkip _::t ->
 *       was_successful t
 *     | RFailure _::_
 *     | RError _::_
 *     | RTodo _::_ ->
 *       false
 *   let _ =
 *     let verbose = ref false in
 *     let set_verbose _ = verbose := true in
 *     Arg.parse
 *       [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
 *       (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
 *       ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
 *     if not (was_successful (QCheck_runner.run_tests_main test_cases)) then
 *       exit 1
 * end *)

open OUnit
open Aws_s3

module type Runtime = sig
  type 'a m

  val run_request :
       region:string
    -> (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end

module TestSuite =
functor
  (Runtime : Runtime)
  ->
  struct
    let region = "ap-southeast-2"

    let create_bucket bucket =
      Runtime.(
        un_m
          (run_request
             ~region
             (module CreateBucket)
             (Types.CreateBucketRequest.make ~bucket ())))

    let delete_bucket bucket =
      Runtime.(
        un_m
          (run_request
             ~region
             (module DeleteBucket)
             (Types.DeleteBucketRequest.make ~bucket ())))

    let list_buckets = Runtime.(un_m (run_request ~region (module ListBuckets) ()))

    let noop_test () =
      let res = create_bucket "test_bucket" in
      "Create s3 bucket"
      @?
      match res with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.CreateBucketOutput.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "SQS noop" >:: noop_test ]

    let rec was_successful = function
      | [] -> true
      | RSuccess _ :: t | RSkip _ :: t -> was_successful t
      | RFailure _ :: _ | RError _ :: _ | RTodo _ :: _ -> false

    let _ =
      let suite = "Tests" >::: test_cases in
      let verbose = ref false in
      let set_verbose _ = verbose := true in
      Arg.parse
        [ "-verbose", Arg.Unit set_verbose, "Run the test in verbose mode." ]
        (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
        ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
      if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
  end
