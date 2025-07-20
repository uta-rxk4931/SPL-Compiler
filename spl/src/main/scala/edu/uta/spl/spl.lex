/********************************************************************************
*
* File: spl.lex
* The SPL scanner
*
********************************************************************************/

package edu.uta.spl;

import java_cup.runtime.Symbol;

%%
%class SplLex
%public
%line
%column
%cup

%{

  private Symbol symbol ( int type ) {
    return new Symbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol ( int type, Object value ) {
    return new Symbol(type, yyline+1, yycolumn+1, value);
  }

  public void lexical_error ( String message ) {
    System.err.println("*** Lexical Error: " + message + " (line: " + (yyline+1)
                       + ", position: " + (yycolumn+1) + ")");
    System.exit(1);
  }
%}

DIGIT=[0-9]
ID=[a-zA-Z][a-zA-Z0-9_]*



%%

/* Literals */
{DIGIT}+                           {
                                      return new Symbol(sym.INTEGER_LITERAL, Integer.valueOf(yytext()));
                                   }

{DIGIT}+"."{DIGIT}*                {
                                        return new Symbol(sym.FLOAT_LITERAL, Float.valueOf(yytext()));
                                   }

\"[^\t\n\"]{0,255}\"               {
                                      return new Symbol(sym.STRING_LITERAL, yytext().substring(1, yytext().length() - 1));
                                   }

/* Keywords */
"array"                            { return symbol(sym.ARRAY); }
"boolean"                          { return new Symbol(sym.BOOLEAN); }
"by"                               { return new Symbol(sym.BY); }
"def"                              { return new Symbol(sym.DEF); }
"else"                             { return new Symbol(sym.ELSE); }
"exit"                             { return new Symbol(sym.EXIT); }
"false"                            { return new Symbol(sym.FALSE); }
"float"                            { return new Symbol(sym.FLOAT); }
"for"                              { return new Symbol(sym.FOR); }
"if"                               { return new Symbol(sym.IF); }
"int"                              { return new Symbol(sym.INT); }
"loop"                             { return new Symbol(sym.LOOP); }
"not"                              { return new Symbol(sym.NOT); }
"print"                            { return new Symbol(sym.PRINT); }
"read"                             { return new Symbol(sym.READ); }
"return"                           { return new Symbol(sym.RETURN); }
"string"                           { return new Symbol(sym.STRING); }
"to"                               { return new Symbol(sym.TO); }
"true"                             { return new Symbol(sym.TRUE); }
"type"                             { return new Symbol(sym.TYPE); }
"var"                              { return new Symbol(sym.VAR); }
"while"                            { return new Symbol(sym.WHILE); }

/* Operators */
"="                                { return new Symbol(sym.EQUAL); }
"+"                                { return new Symbol(sym.PLUS); }
"-"                                { return new Symbol(sym.MINUS); }
"*"                                { return new Symbol(sym.TIMES); }
"/"                                { return new Symbol(sym.DIV); }
"%"                                { return new Symbol(sym.MOD); }
"=="                               { return new Symbol(sym.EQ); }
"<>"                               { return new Symbol(sym.NEQ); }
"<"                                { return new Symbol(sym.LT); }
"<="                               { return new Symbol(sym.LEQ); }
">"                                { return new Symbol(sym.GT); }
">="                               { return new Symbol(sym.GEQ); }
"&&"                               { return new Symbol(sym.AND); }
"||"                               { return new Symbol(sym.OR); }

/* Delimiters */
":"                                { return new Symbol(sym.COLON); }
";"                                { return new Symbol(sym.SEMI); }
","                                { return new Symbol(sym.COMMA); }
"."                                { return new Symbol(sym.DOT); }
"("                                { return new Symbol(sym.LP); }
")"                                { return new Symbol(sym.RP); }
"{"                                { return new Symbol(sym.LB); }
"}"                                { return new Symbol(sym.RB); }
"["                                { return new Symbol(sym.LSB); }
"]"                                { return new Symbol(sym.RSB); }
"#"                                { return new Symbol(sym.SHARP); }

/* Identifiers */
{ID}                    { return new Symbol(sym.ID,yytext()); }

/* Whitespace */
[ \t\r\n\f]             { /* ignore white spaces. */ }

/* comments */
"/*" ([^*] | \*+[^*/])* \*+ "/"        { /* Ignore comments */ }

/* Error */
.               { lexical_error("Illegal character"); }
