type storage = {
  map : (int, (int,bool) map) map;
  enjeu : tez;
  player1 : (unit, unit) contract;
  player2 : (unit, unit) contract;
  }
type parameter = { play : bool; x : int; y : int }

let contract
      ( storage : storage )
      ( parameter : parameter )
      : (bool option) option
  =
  let s = "This is a Tic Tac Toe game. Still work in progress" in

  (*  global = (pair (pair tez parameter) storage) *)
  (*  storage = (pair board payback) *)

  let x = parameter.x in
  let y = parameter.y in

  if x < 0 or x > 2 then Current.failwith ();
  if y < 0 or y > 2 then Current.failwith ();

  (*
       DUP ; CADR                 (*  play : global *)
       DIP { CDR } ;              (*  play : (pair board contracts) *)
       DIP { DUP ; CAR ; DIP { CDR } } (*  play : board : contracts *)

       (*  check that X and Y are between 0 and 2 *)
       DUP ; CDR ; DUP ; CAR ; DIP { CDR } (*  X : Y : play : board *)

       DUP ;
       PUSH int 2 ; IFCMPLT { FAIL } { } ;
       PUSH int 0 ; IFCMPGT { FAIL } { } ;
       DUP ;
       PUSH int 2 ; IFCMPLT { FAIL } { } ;
       PUSH int 0 ; IFCMPGT { FAIL } { } ;
   *)

  let counter ( x : (int * (int, bool) map) * int) =
    let x0 = x.(0) in
    let x1 = x.(1) in
    let map = x0.(1) in
    Map.reduce (fun (arg : (int * bool) * int) ->
        let x = arg.(1) in
        x+1)
               map
               x1
  in
  (*
       LAMBDA (*  counter *)
         (pair (pair int (map int bool)) int)
         int
         { DUP; CAR; CDR; DIP { CDR };
           LAMBDA (*  succ *)
             (pair (pair int bool) int)
             int
             { CDR ; PUSH int 1; ADD }
           REDUCE
         }
   *)

(*
       (*  counter : play : board : contracts *)
       DIP { DUUP; DIP { PUSH int 0 } };
       REDUCE; (*  count : play : board : contracts *)
       DIP { PUSH int 2 }; MOD; PUSH int 0
       IFCMPEQ
        { (*  if the number of plays mod 2 is 0 then the first player can play *)
          DUP ; CAR ;
          IF { } { FAIL }
        }
        { (*  otherwise the second player can play *)
          DUP ; CAR ;
          IF { FAIL } { }
        }

       (*  If this is the first player playing, increase the amount to reimburse to him *)
       (*  play : board : contracts *)
       DUP ; CAR ; SWAP ; DIP { SWAP } ;
       (*  play : board : player : contracts *)
       DIIP
        { IF { DUP ; CAR ; DIP { CDR } ;
               (*  amount : contracts *)
               AMOUNT ; ADD ;
               PAIR ;
               (*  Check that the player is the first player *)
               DUP ; CDR ; CAR ; MANAGER ;
               SOURCE unit unit; MANAGER ; (*  source_manager : contract1_manager : contracts *)
               IFCMPEQ { } { FAIL }
             }
           {   (*  Check that the player is the first player *)
               DUP ; CDR ; CDR ; MANAGER ;
               SOURCE unit unit; MANAGER ; (*  source_manager : contract2_manager : contracts *)
               IFCMPEQ { } { FAIL }
           }
        }

       PAIR ;                     (*  (play, board) *)
       LAMBDA                     (*  update_board : (play, board) *)
         (pair (pair bool (pair int int)) (map int (map int bool)))
         (map int (map int bool))
         { DUP ; CAR ; DIP { CDR } ;  (*  (pair player (pair X Y)) : (map int (map int bool)) *)
           DUP ; CAR ; DIP { CDR } ;  (*  player : (pair X Y) : (map int (map int bool)) *)
           DIP { DUP ; DIP { DIP { DUP }; SWAP } }
                                      (*  player : (pair X Y) : (map int (map int bool)) *)
                                      (*         : (pair X Y) : (map int (map int bool)) *)
           DIP { DUP ; CAR ; DIP { CDR } ; (*  X : Y : (map int (map int bool)) *)
                 DIP { SWAP } ;            (*  X : (map int (map int bool)) : Y *)
                 GET ;                     (*  (option (map int bool)) : Y *)
                 IF_NONE { EMPTY_MAP int bool } { }
                 SWAP ;
                 DIP { DUP } ;
                 GET ;                     (*  (option bool) : (map int bool) *)
                 IF_NONE {} { FAIL }
               };
             (*  player : (map int bool) : (pair X Y) : (map int (map int bool)) *)
           SOME ;
           DUUUP ; CDR ;
             (*  Y : Some player : (map int bool) : (pair X Y) : (map int (map int bool)) *)
           UPDATE ;
             (*  (map int bool) : (pair X Y) : (map int (map int bool)) *)
           SOME ;
           SWAP ; CAR ;
             (*  X : Some (map int bool) : (map int (map int bool)) *)
           UPDATE
         }

                        (*  update_board : (play, board) *)
       SWAP ; EXEC ;    (*  updated_board *)

       LAMBDA
         (pair (list (pair int int)) (pair (option bool) (map int (map int bool))))
         (pair (option bool) (map int (map int bool)))
         { DUP ; CAR ; DIP { CDR; DUP ; CDR } ; (*  (list (pair int int)) : (map int (map int bool)) : (pair (option bool) (map int (map int bool))) *)
           LAMBDA
             (pair (pair int int)
                   (pair int (map int (map int bool))))
             (pair int (map int (map int bool)))
             { DUP ; CAR ; DIP { CDR; DUP ; CAR ; DIP { CDR ; DUP } ; SWAP } ;
               (*  (pair int int) : (map int (map int bool)) : int : (map int (map int bool)) *)
               PAIR ;
               LAMBDA                    (*  get_position : ... *)
                 (pair (pair int int) (map int (map int bool)))
                 (option bool)
                 { DUP ; CAR ; DIP { CDR } ; (*  (pair X Y) : (map int (map int bool)) *)
                   DUP ; CAR ; DIP { CDR } ; (*  X : Y : (map int (map int bool)) *)
                   DIP { SWAP } ;            (*  X : (map int (map int bool)) : Y *)
                   GET ;                     (*  (option (map int bool)) : Y *)
                   IF_NONE { EMPTY_MAP int bool } { }
                   SWAP ;
                   GET ;
                 }
               SWAP ;
               EXEC ;
               IF_NONE { PUSH int 0 } { IF { PUSH int 1 } { PUSH int -1 } } ;
               ADD ;
               PAIR ;
             }
           DIIP { PUSH int 0; PAIR } ;
           REDUCE ;
           CAR ;
           DUP ;
           PUSH int 3;
           IFCMPEQ
             { PUSH bool True ; SOME ; SWAP ; DROP }
             { PUSH int -3;
               IFCMPEQ
                 { PUSH bool False ; SOME }
                 { NONE bool } }
           DIP { DUP ; CAR ; DIP { CDR } }
           (*  new (option bool) : old (option bool) : board *)
           IF_NONE {} { DIP { DROP }; SOME }
           PAIR
         }
       (*  check_win : updated_board *)
       DIP { NONE bool ; PAIR ; }
       (*  check_win : (pair none updated_board) *)
       PUSH (list (list (pair int int)))
         (List
           (List (Pair 0 0) (Pair 1 0) (Pair 2 0))
           (List (Pair 0 1) (Pair 1 1) (Pair 2 1))
           (List (Pair 0 2) (Pair 1 2) (Pair 2 2))

           (List (Pair 0 0) (Pair 0 1) (Pair 0 2))
           (List (Pair 1 0) (Pair 1 1) (Pair 1 2))
           (List (Pair 2 0) (Pair 2 1) (Pair 2 2))

           (List (Pair 0 0) (Pair 1 1) (Pair 2 2))
           (List (Pair 0 2) (Pair 1 1) (Pair 2 0))) ;
       SWAP ;
       REDUCE ;
       DUP ; CAR ; DIP { CDR } ;
       IF_NONE
         { LAMBDA (*  count *)
             (pair (pair int (map int bool)) int)
             int
             { DUP; CAR; CDR; DIP { CDR };
               LAMBDA (*  succ *)
                 (pair (pair int bool) int)
                 int
                 { CDR ; PUSH int 1; ADD }
               REDUCE
             }
           DIP { DUP; DIP { PUSH int 0 } };
           REDUCE; (*  count : updated_board : payback *)
           PUSH int 9;
           IFCMPEQ
             { (*  draw: payback players *)
               PAIR ;
               DUP ;
               CDR ; (*  payback : (pair updated_board payback) *)
               DUP ;
               CDR ; CAR ; SWAP ; CAR ;
               PUSH unit Unit;
               TRANSFER_TOKENS;
               DROP;
               DUP ;
               CDR ; (*  payback : (pair updated_board payback) *)
               CDR ; CDR ; (*  contract2 : (pair updated_board payback) *)
               PUSH tez "2";
               BALANCE ;
               SUB ;
               PUSH unit Unit;
               TRANSFER_TOKENS;
               DROP ;
               NONE bool;
               LEFT unit;
             }
             { (*  still playing *)
               PAIR;
               UNIT;
               RIGHT (option bool);
             }
         }
         { IF
            { (*  First player wins *)
              PAIR;
              DUP ; CDR ; (*  payback : (pair updated_board payback) *)
              CDR ; CAR ; (*  contract1 : (pair updated_board payback) *)
              PUSH tez "2";
              BALANCE ;
              SUB ;
              PUSH unit Unit;
              TRANSFER_TOKENS;
              DROP ;
              PUSH bool True;
            }
            { (*  Second player wins *)
              PAIR;
              DUP ; CDR ; (*  payback : (pair updated_board payback) *)
              CDR ; CDR ; (*  contract2 : (pair updated_board payback) *)
              PUSH tez "2";
              BALANCE ;
              SUB ;
              PUSH unit Unit;
              TRANSFER_TOKENS;
              DROP ;
              PUSH bool False;
            }
           SOME; LEFT unit; }
       PAIR
  }
   *)
  ( (None :  (bool option) option) , storage )
