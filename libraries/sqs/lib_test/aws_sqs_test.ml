open OUnit
open Aws_sqs

let from_opt = function
  | None -> assert false
  | Some(x) -> x

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
      (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let delete_queue queue_url =
    Runtime.(un_m (run_request
                     (module DeleteQueue)
                     (Types.DeleteQueueRequest.make ~queue_url ())))

  (* TODO Tag created queues with project name for later cleanup / tracking *)
  let create_queue queue_name =
    Runtime.(un_m (run_request
                     (module CreateQueue)
                     (Types.CreateQueueRequest.make ~queue_name ())))

  let send_message queue_url message_body =
    Runtime.(un_m (run_request
                     (module SendMessage)
                     (Types.SendMessageRequest.make ~queue_url ~message_body ())))

  let receive_message queue_url =
    Runtime.(un_m (run_request
                     (module ReceiveMessage)
                     (Types.ReceiveMessageRequest.make ~queue_url ())))

  let list_queues ~queue_name_prefix =
    Runtime.(un_m (run_request
                     (module ListQueues)
                     (Types.ListQueuesRequest.make ~queue_name_prefix ())))
  (*
     This only generates simple queue names, it should generate the
     full range of allowable queue_names as per the AWS spec.

     QueueName
     The name of the new queue. The following limits apply to this name:

     A queue name can have up to 80 characters.
     Valid values: alphanumeric characters, hyphens (-), and underscores (_).
     A FIFO queue name must end with the .fifo suffix.
   *)
  let arb_queue_name =
    QCheck.Gen.oneofl Aws_test.Corpus.cooking

  (*
     Immprove this to generate the full range of valid messages.

     http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
     invalid unicode values #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF]

     let invalid = P.concat [['\x9'],['\xA'],['\xD'], ['\x20'..'\xD7FF'], ['\xE000'..'\xFFFD'], ['\x10000'..'\x10FFFF']]
     in (pack . List.filter (\x -> P.elem x invalid) <$> utf8_char)
   *)
  let arb_message =
    QCheck.Gen.oneofl Aws_test.Corpus.agile

  let send_receive_message_test =
    QCheck.Test.make ~count:1
      ~name:"SQS create / delete queue"
      QCheck.(QCheck.make @@ QCheck.Gen.pair arb_queue_name arb_message)
      (fun (queue_name, test_message) ->
        let create_res = create_queue queue_name in

        match create_res with
        | `Ok resp ->
           Printf.printf "%s\n" (Yojson.Basic.to_string (Types.CreateQueueResult.(to_json (of_json (to_json resp)))));
           true
        | `Error err ->
           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err)
          ;
        let queue_url = match create_res with
          | `Ok resp -> from_opt resp.queue_url
          | `Error err -> assert false
        in
        let send_message = send_message queue_url test_message in
        match send_message with
        | `Ok resp -> true
        | `Error err ->
           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err)
          ;
        let receive_message = receive_message queue_url in
        match receive_message with
        | `Ok resp -> true
        | `Error err ->
           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err)
          ;
        (* TODO Delete the message here. *)
        let delete_res = delete_queue queue_url in
           match delete_res with
           | `Ok resp -> true
           | `Error err ->
              Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
              false
      )

  let create_delete_queue_test =
    QCheck.Test.make ~count:1
    ~name:"SQS create / delete queue"
    QCheck.(QCheck.make arb_queue_name)
    (fun queue_name ->
      let create_res = create_queue queue_name in

      match create_res with
         | `Ok resp ->
            Printf.printf "%s\n" (Yojson.Basic.to_string (Types.CreateQueueResult.(to_json (of_json (to_json resp)))));
            true
         | `Error err ->
            Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err)
           ;
      let queue_url = match create_res with
        | `Ok resp -> from_opt resp.queue_url
        | `Error err -> assert false
      in

      let delete_res = delete_queue queue_url in
      match delete_res with
      | `Ok resp -> true
      | `Error err ->
         Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
         false
    )
  let create_list_queue_test =
    QCheck.Test.make ~count:1
      ~name:"SQS create many queues and list"
      QCheck.(QCheck.make ~print:(fun (x,y) -> "(" ^ x ^ "," ^ y ^ ")") @@
                QCheck.Gen.pair arb_queue_name arb_queue_name)
      (fun (queue_name_1, queue_name_2) ->
        let queue_name_prefix = "aws-sqs-test-" in
        let _create_res_1 = create_queue @@ queue_name_prefix ^ queue_name_1 in
        let _create_res_2 = create_queue @@ queue_name_prefix ^ queue_name_2 in
        let list_queue_res = list_queues ~queue_name_prefix in

        match list_queue_res with
        | `Ok resp ->
           Printf.printf "List queues: %s\n"  (Yojson.Basic.to_string (Types.QueueUrlList.to_json resp.queue_urls));
           let _ = List.map delete_queue resp.queue_urls in
           List.length resp.queue_urls == 2
        | `Error err ->
           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
           false
      )

  let test_cases =
    [ create_delete_queue_test
    ; send_receive_message_test
    ; create_list_queue_test
    ]

  let rec was_successful =
    function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
      was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
      false
  let _ =
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (QCheck_runner.run_tests_main test_cases)) then
      exit 1
end
