// parser.mly

%token                 EOF
%token <int>           LITINT
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




%%
