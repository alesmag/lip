%{
    open Rule
%}

%token <int> INT
%token S
%token B
%token E
%token SLASH
%token COMMA
%token WHITE
%token DOTS
%token EOF

%start <Rule.rule> prog

%%

prog:
    | srule = S_rule; SLASH; brule = B_rule; EOF { { survive = srule; birth = brule } }
    | E; srule = E_S_rule; SLASH; brule = E_B_rule; EOF { { survive = srule; birth = brule } }
    | E; WHITE; srule = E_S_rule; WHITE; SLASH; WHITE; brule = E_B_rule; EOF { { survive = srule; birth = brule } }
;

S_rule:
    | S; nums = num_list { nums }
;

B_rule:
    | B; nums = num_list { nums }
;

num_list:
    | { [] }
    | digit = INT; rest = num_list { digit :: rest}
;

E_S_rule:
    | S; nums = E_num_list { nums }
    | nums = E_num_list { nums }
;

E_B_rule:
    | B; nums = E_num_list { nums }
    | nums = E_num_list { nums }
;

E_num_list:
    | { [] }
    | digit = INT; rest = num_list { digit :: rest}
    | digit = INT; COMMA; rest = num_list { digit :: rest }
    | digit = INT; DOTS; rest = num_list { digit :: rest }