(* Parser *)

%{
    open Ast
%}

/* Constants */
%token <string> STRING ATOM

/* Variables */
%token <string> VAR

/* Symbols */
%token LPAREN
%token RPAREN
%token COMMA
%token PERIOD
%token RULE
%token QUERY

/* Start */
%start clause

/* Types */
%type <Ast.dec> clause
%type <Ast.exp> predicate term structure
%type <Ast.exp list> term_list predicate_list
%type <Ast.const> constant

%%

clause:
    | p = predicate; RULE; p1 = predicate_list; PERIOD
        { Clause (p, p1) }
    | p = predicate; PERIOD
        { fact_sugar p }
    | QUERY; p1 = predicate_list; PERIOD
        { Query p1 }

predicate_list:
    | p = predicate
        { [p] }
    | p = predicate; COMMA; p1 = predicate_list
        { p :: p1 }

predicate:
    | a = ATOM
        { atom_sugar a }
    | s = structure
        { s }

structure:
    | a = ATOM; LPAREN; RPAREN
        { atom_sugar a }
    | a = ATOM; LPAREN; t1 = term_list; RPAREN
        { TermExp (a, t1) }

term_list:
    | t = term
        { [t] }
    | t = term; COMMA; t1 = term_list
        { t :: t1 }

term:
    | c = constant
        { ConstExp c }
    | a = ATOM
        { atom_sugar a }
    | v = VAR
        { VarExp v }
    | s = structure
        { s }

constant:
    | s = STRING
        {String s}

