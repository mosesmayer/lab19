(*
                Component Behaviors of an ATM Machine

The functions here represent the component behaviors that an ATM
machine can take, including: prompting for and acquiring from the
customer some information (choosing an action or entering an account
id or an amount); presenting information to the customer; dispensing
cash.

Implementation of these behaviors is likely to require some database
of accounts, each with an id number, a customer name, and a current
balance.
 *)

(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, arbitrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
val initialize : account_spec list -> unit ;;

open Printf
(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let rec acquire_id () : id =
  printf "Enter id: ";
    let tmp = int_of_string_opt (read_line ()) in
    match tmp with
    | None -> printf "Invalid id"; acquire_id ()
    | Some x -> 
      if List.exists (fun u -> u.id = x) !users then x
      else (printf "Invalid id"; acquire_id ())

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let rec acquire_amount () : int =
  let x = int_of_string_opt (read_line ()) in
    match x with
    | Some y -> y
    | None -> (printf "Invalid amount\n"; acquire_amount ())
;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let rec acquire_act () : action =
  printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit:";
  let tmp = read_line () in
  match tmp with
  | "B" -> Balance
  | "=" -> Next
  | "X" -> Finished
  | "-" | "+" ->
    let value = acquire_amount () in
    if tmp = "+" then Deposit value else Withdraw value
  | "" | _ -> (printf "Invalid action.\n"; acquire_act ())
;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (i : id) : int =
  let usr = List.find (fun x -> x.id = i) !users in
  usr.id

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (i : id) : string =
  let usr = List.find (fun x -> x.id = i) !users in
  usr.name

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance (i : id) (v : int) : unit =
  let usr = List.find (fun x -> x.id = i) !users in
  usr.balance := v;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
let present_message (s : string) : unit =
  printf "%s\n" s
;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash (value : int) : unit =
  printf "Here\'s your cash: %d\n" value
;;

