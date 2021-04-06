(* Test syntax analyser *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let tree = Absyntree.flat_nodes (Absyntree.tree_of_lfundecs ast) in
    let box = Tree.box_of_tree tree in
    Format.printf "%s\n\n%!" (Box.string_of_box box);
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test _ =
  (* function declaration and constant expression *)
  check "int f(int x) = 100";
  [%expect{|
           ╭────────────╮           
           │Program tree│           
           ╰──────┬─────╯           
               ╭──┴╮                
               │Fun│                
               ╰──┬╯                
     ╭───────────┬┴───────────╮     
╭────┴────╮  ╭───┴───╮  ╭─────┴────╮
│    f    │  │Formals│  │IntExp 100│
│Absyn.Int│  ╰───┬───╯  ╰──────────╯
╰─────────╯ ╭────┴────╮             
            │    x    │             
            │Absyn.Int│             
            ╰─────────╯  |}];

  check "int f(int x, int y, bool z) = 100";
  [%expect{|
                       ╭────────────╮                        
                       │Program tree│                        
                       ╰──────┬─────╯                        
                            ╭─┴─╮                            
                            │Fun│                            
                            ╰─┬─╯                            
     ╭────────────────────────┴────────────────────────╮     
╭────┴────╮              ╭────┴──╮               ╭─────┴────╮
│    f    │              │Formals│               │IntExp 100│
│Absyn.Int│              ╰────┬──╯               ╰──────────╯
╰─────────╯      ╭───────────┬┴───────────╮                  
            ╭────┴────╮ ╭────┴────╮ ╭─────┴────╮             
            │    x    │ │    y    │ │    z     │             
            │Absyn.Int│ │Absyn.Int│ │Absyn.Bool│             
            ╰─────────╯ ╰─────────╯ ╰──────────╯             |}];

  check "int f() = 100";
  [%expect{| :1.7 error: syntax |}];

  check "foo f(int x) = 100";
  [%expect{| :1.3 error: syntax |}];

  (* binary operators *)
  check "bool f(int x) = 2 + 3 + 4 < 5 + 6";
  [%expect{|
                                    ╭────────────╮
                                    │Program tree│
                                    ╰──────┬─────╯
                                         ╭─┴─╮
                                         │Fun│
                                         ╰─┬─╯
          ╭───────────┬────────────────────┴────────────╮
    ╭─────┴────╮  ╭───┴───╮                        ╭────┴──╮
    │    f     │  │Formals│                        │OpExp <│
    │Absyn.Bool│  ╰───┬───╯                        ╰────┬──╯
    ╰──────────╯ ╭────┴────╮                 ╭──────────┴───────────────╮
                 │    x    │            ╭────┴──╮                   ╭───┴───╮
                 │Absyn.Int│            │OpExp +│                   │OpExp +│
                 ╰─────────╯            ╰────┬──╯                   ╰───┬───╯
                                       ╭─────┴──────────╮          ╭────┴─────╮
                                   ╭───┴───╮       ╭────┴───╮ ╭────┴───╮ ╭────┴───╮
                                   │OpExp +│       │IntExp 4│ │IntExp 5│ │IntExp 6│
                                   ╰───┬───╯       ╰────────╯ ╰────────╯ ╰────────╯
                                  ╭────┴─────╮
                             ╭────┴───╮ ╭────┴───╮
                             │IntExp 2│ │IntExp 3│
                             ╰────────╯ ╰────────╯ |}];

  check "bool f(int x) = 2 < 3 < 4";
  [%expect{| :1.23 error: syntax |}];

  (*id*)

  check "int exp (int x) = z";
  [%expect{|
             ╭────────────╮
             │Program tree│
             ╰──────┬─────╯
                  ╭─┴─╮
                  │Fun│
                  ╰─┬─╯
         ╭──────────┴┬──────────╮
    ╭────┴────╮  ╭───┴───╮  ╭───┴───╮
    │   exp   │  │Formals│  │IdExp z│
    │Absyn.Int│  ╰───┬───╯  ╰───────╯
    ╰─────────╯ ╭────┴────╮
                │    x    │
                │Absyn.Int│
                ╰─────────╯ |}];

(*conditional*)

  check "int exp (int x) = if 5 < 1 then 4 else 2 ";
  [%expect{|
                              ╭────────────╮
                              │Program tree│
                              ╰──────┬─────╯
                                   ╭─┴─╮
                                   │Fun│
                                   ╰─┬─╯
         ╭───────────┬───────────────┴───────────╮
    ╭────┴────╮  ╭───┴───╮                 ╭─────┴────╮
    │   exp   │  │Formals│                 │CondExp if│
    │Absyn.Int│  ╰───┬───╯                 ╰─────┬────╯
    ╰─────────╯ ╭────┴────╮           ╭──────────┴─────┬──────────╮
                │    x    │       ╭───┴───╮       ╭────┴───╮ ╭────┴───╮
                │Absyn.Int│       │OpExp <│       │IntExp 4│ │IntExp 2│
                ╰─────────╯       ╰───┬───╯       ╰────────╯ ╰────────╯
                                 ╭────┴─────╮
                            ╭────┴───╮ ╭────┴───╮
                            │IntExp 5│ │IntExp 1│
                            ╰────────╯ ╰────────╯ |}];

(*function call*)
check "int exp (int x) = f(1,2,3)";
[%expect{|
                       ╭────────────╮
                       │Program tree│
                       ╰──────┬─────╯
                           ╭──┴╮
                           │Fun│
                           ╰──┬╯
       ╭───────────┬──────────┴───────────╮
  ╭────┴────╮  ╭───┴───╮          ╭───────┴─────╮
  │   exp   │  │Formals│          │FuncCallExp f│
  │Absyn.Int│  ╰───┬───╯          ╰───────┬─────╯
  ╰─────────╯ ╭────┴────╮      ╭──────────┴──────────╮
              │    x    │ ╭────┴───╮ ╭────┴───╮ ╭────┴───╮
              │Absyn.Int│ │IntExp 1│ │IntExp 2│ │IntExp 3│
              ╰─────────╯ ╰────────╯ ╰────────╯ ╰────────╯ |}];

(*declaration*)
check "int exp (int x) = let y = 10 in z";
[%expect{|
                 ╭────────────╮
                 │Program tree│
                 ╰──────┬─────╯
                      ╭─┴─╮
                      │Fun│
                      ╰─┬─╯
       ╭───────────┬────┴───────────╮
  ╭────┴────╮  ╭───┴───╮   ╭────────┴───────╮
  │   exp   │  │Formals│   │DeclarationExp y│
  │Absyn.Int│  ╰───┬───╯   ╰────────┬───────╯
  ╰─────────╯ ╭────┴────╮      ╭────┴─────╮
              │    x    │ ╭────┴────╮ ╭───┴───╮
              │Absyn.Int│ │IntExp 10│ │IdExp z│
              ╰─────────╯ ╰─────────╯ ╰───────╯ |}];

(*all together*)
check "int exp (int x) = z(1,2)\n int exp (int x) = if 0 < 1 then z else 3\n int exp (int x) = let var = 10 in exp ";
[%expect{|
                                                                           ╭────────────╮
                                                                           │Program tree│
                                                                           ╰──────┬─────╯
                        ╭────────────────────────────────────────────────────────┬┴───────────────────────────────────────────────────────╮
                      ╭─┴─╮                                                   ╭──┴╮                                                     ╭─┴─╮
                      │Fun│                                                   │Fun│                                                     │Fun│
                      ╰─┬─╯                                                   ╰──┬╯                                                     ╰─┬─╯
       ╭───────────┬────┴───────────╮                ╭───────────┬───────────────┴───────────╮                          ╭───────────┬─────┴───────────╮
  ╭────┴────╮  ╭───┴───╮     ╭──────┴──────╮    ╭────┴────╮  ╭───┴───╮                 ╭─────┴────╮                ╭────┴────╮  ╭───┴───╮   ╭─────────┴────────╮
  │   exp   │  │Formals│     │FuncCallExp z│    │   exp   │  │Formals│                 │CondExp if│                │   exp   │  │Formals│   │DeclarationExp var│
  │Absyn.Int│  ╰───┬───╯     ╰──────┬──────╯    │Absyn.Int│  ╰───┬───╯                 ╰─────┬────╯                │Absyn.Int│  ╰───┬───╯   ╰─────────┬────────╯
  ╰─────────╯ ╭────┴────╮      ╭────┴─────╮     ╰─────────╯ ╭────┴────╮           ╭──────────┴────┬──────────╮     ╰─────────╯ ╭────┴────╮      ╭─────┴─────╮
              │    x    │ ╭────┴───╮ ╭────┴───╮             │    x    │       ╭───┴───╮       ╭───┴───╮ ╭────┴───╮             │    x    │ ╭────┴────╮ ╭────┴────╮
              │Absyn.Int│ │IntExp 1│ │IntExp 2│             │Absyn.Int│       │OpExp <│       │IdExp z│ │IntExp 3│             │Absyn.Int│ │IntExp 10│ │IdExp exp│
              ╰─────────╯ ╰────────╯ ╰────────╯             ╰─────────╯       ╰───┬───╯       ╰───────╯ ╰────────╯             ╰─────────╯ ╰─────────╯ ╰─────────╯
                                                                             ╭────┴─────╮
                                                                        ╭────┴───╮ ╭────┴───╮
                                                                        │IntExp 0│ │IntExp 1│
                                                                        ╰────────╯ ╰────────╯ |}];