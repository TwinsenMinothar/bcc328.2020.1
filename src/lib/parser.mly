// parser.mly

%token                 EOF
%token <int>           LITINT
<<<<<<< HEAD
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

%start <Absyn.lfundec> program

%nonassoc LT
%left PLUS
=======
%token                 PLUS
%token                 INT
%token                 BOOL
%token                 LET
%token                 IN
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LT
%token                 LPAREN
%token                 RPAREN
%token                 COMMA
%token                 EQ
%token <Symbol.symbol> ID



>>>>>>> add: tokens

%%

program:
| x=fundec EOF { x }

exp:
| x=LITINT                { $loc , Absyn.IntExp x }
| x=exp op=operator y=exp { $loc , Absyn.OpExp (op, x, y) }

%inline operator:
| PLUS { Absyn.Plus }
| LT   { Absyn.LT }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }
