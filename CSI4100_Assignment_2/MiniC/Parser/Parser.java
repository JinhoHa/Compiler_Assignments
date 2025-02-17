package MiniC.Parser;


import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.Scanner.Scanner;
import MiniC.ErrorReporter;

public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
    currentToken = scanner.scan();
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if (token == Token.VOID ||
      token == Token.INT  ||
      token == Token.BOOL ||
      token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }

  boolean isRelationalToken(int token) {
    if (token == Token.EQ ||
      token == Token.NOTEQ  ||
      token == Token.LESSEQ ||
      token == Token.LESS ||
      token == Token.GREATEREQ ||
      token == Token.GREATER) {
      return true;
    } else {
      return false;
    }
  }

  boolean isAddToken(int token) {
    if (token == Token.PLUS ||
      token == Token.MINUS) {
      return true;
    } else {
      return false;
    }
  }

  boolean isMultToken(int token) {
    if (token == Token.TIMES ||
      token == Token.DIV) {
      return true;
    } else {
      return false;
    }
  }

  boolean isUnaryToken(int token) {
    if (token == Token.PLUS ||
      token == Token.MINUS ||
      token == Token.NOT) {
      return true;
    } else {
      return false;
    }
  }

  boolean isFirstExpr(int token) {
    if (token == Token.PLUS ||
      token == Token.MINUS ||
      token == Token.NOT ||
      token == Token.ID ||
      token == Token.LEFTPAREN ||
      token == Token.INTLITERAL ||
      token == Token.BOOLLITERAL ||
      token == Token.FLOATLITERAL ||
      token == Token.STRINGLITERAL) {
      return true;
    } else {
      return false;
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parse() {

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) {return; /* to be refined in Assignment 3...*/ }
    return;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseProgram() throws SyntaxError {
    while (isTypeSpecifier(currentToken.kind)) {
      acceptIt();
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN) {
        parseFunPart();
      } else {
        parseVarPart();
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseFunPart() throws SyntaxError {
    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    if (isTypeSpecifier(currentToken.kind)) {
      parseParamsList();
    }
    accept(Token.RIGHTPAREN);
    parseCompoundStmt();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParamsDecl ( "," ParamsDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseParamsList() throws SyntaxError {
    // to be completed by you...
    parseParamsDecl();
    while(currentToken.kind == Token.COMMA) {
      acceptIt();
      parseParamsDecl();
    }

  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsDecl():
  //
  // ParamsDecl ::= TypeSpecifier Declarator
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseParamsDecl() throws SyntaxError {
    if(isTypeSpecifier(currentToken.kind)) {
      acceptIt();
      parseDeclarator();
    }
    else {
      syntaxError("\"%\" not expected here, TypeSpecifier expected",
          currentToken.GetLexeme());
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseDeclarator():
  //
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseDeclarator() throws SyntaxError {
    accept(Token.ID);
    if(currentToken.kind == Token.LEFTBRACKET) {
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" variable-def* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseCompoundStmt() throws SyntaxError {
    // to be completed by you...
    accept(Token.LEFTBRACE);
    while(isTypeSpecifier(currentToken.kind)) {
      parseVarDef();
    }
    while(currentToken.kind == Token.LEFTBRACE ||
    currentToken.kind == Token.IF ||
    currentToken.kind == Token.WHILE ||
    currentToken.kind == Token.FOR ||
    currentToken.kind == Token.RETURN ||
    currentToken.kind == Token.ID) {
      parseStmt();
    }
    accept(Token.RIGHTBRACE);

  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseStmt():
  //
  // Stmt ::= CompoundStmt
  //        | if-stmt
  //        | while-stmt
  //        | for-stmt
  //        | return expr? ";"
  //        | ID ( ( "=" expr ) | ( "[" expr "]" "=" expr ) | ( arglist ) ) ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseStmt() throws SyntaxError {
    switch(currentToken.kind) {
      case Token.LEFTBRACE:
        parseCompoundStmt();
        break;
      case Token.IF:
        parseIfStmt();
        break;
      case Token.WHILE:
        parseWhileStmt();
        break;
      case Token.FOR:
        parseForStmt();
        break;
      case Token.RETURN:
        accept(Token.RETURN);
        if(isFirstExpr(currentToken.kind)) {
          parseExpr();
        }
        accept(Token.SEMICOLON);
        break;
      case Token.ID:
        accept(Token.ID);
        switch(currentToken.kind) {
          case Token.ASSIGN:
            accept(Token.ASSIGN);
            parseExpr();
            break;
          case Token.LEFTBRACKET:
            accept(Token.LEFTBRACKET);
            parseExpr();
            accept(Token.RIGHTBRACKET);
            accept(Token.ASSIGN);
            parseExpr();
            break;
          case Token.LEFTPAREN:
            parseArgList();
            break;
          default:
            syntaxError("\"%\" not expected here",
              currentToken.GetLexeme());
        }
        accept(Token.SEMICOLON);
        break;
      default:
        syntaxError("\"%\" not expected here",
          currentToken.GetLexeme());
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseWhileStmt():
  //
  // while-stmt ::= while "(" expr ")" stmt
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseWhileStmt() throws SyntaxError {
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    parseExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseForStmt():
  //
  // for-stmt ::= for "(" asgnexpr? ";" expr? ";" asgnexpr? ")" stmt
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseForStmt() throws SyntaxError {
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    if(currentToken.kind == Token.ID) {
      parseAsgnExpr();
    }
    accept(Token.SEMICOLON);
    if(isFirstExpr(currentToken.kind)) {
      parseExpr();
    }
    accept(Token.SEMICOLON);
    if(currentToken.kind == Token.ID) {
      parseAsgnExpr();
    }
    accept(Token.RIGHTPAREN);
    parseStmt();
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseIfStmt():
  //
  // if-stmt ::= if "(" expr ")" stmt ( else stmt )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseIfStmt() throws SyntaxError {
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    parseExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
    if(currentToken.kind == Token.ELSE) {
      accept(Token.ELSE);
      parseStmt();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarDef():
  //
  // variable-def ::= TypeSpecifier init-decl ("," init-decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseVarDef() throws SyntaxError {
    if(isTypeSpecifier(currentToken.kind)) {
      acceptIt();
    }
    else {
      syntaxError("\"%\" not expected here",
          currentToken.GetLexeme());
    }

    parseInitDecl();

    while(currentToken.kind == Token.COMMA) {
      accept(Token.COMMA);
      parseInitDecl();
    }

    accept(Token.SEMICOLON);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseVarPart() throws SyntaxError {
    if(currentToken.kind == Token.LEFTBRACKET) {
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
    if(currentToken.kind == Token.ASSIGN) {
      accept(Token.ASSIGN);
      parseInitializer();
    }
    while(currentToken.kind == Token.COMMA) {
      accept(Token.COMMA);
      parseInitDecl();
    }
    accept(Token.SEMICOLON);

  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseInitDecl():
  //
  // InitDecl ::= Declarator ( "=" Initializer )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseInitDecl() throws SyntaxError {
    parseDeclarator();
    if(currentToken.kind == Token.ASSIGN) {
      accept(Token.ASSIGN);
      parseInitializer();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseInitializer():
  //
  // Initializer ::= expr
  //               | "{" expr ( "," expr)* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseInitializer() throws SyntaxError {
    if(currentToken.kind == Token.LEFTBRACE) {
      accept(Token.LEFTBRACE);
      parseExpr();
      while(currentToken.kind == Token.COMMA) {
        accept(Token.COMMA);
        parseExpr();
      }
      accept(Token.RIGHTBRACE);
    }
    else {
      parseExpr();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseExpr():
  //
  // expr ::= and-expr ( "||" and-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseExpr() throws SyntaxError {
    parseAndExpr();
    while(currentToken.kind == Token.OR) {
      accept(Token.OR);
      parseAndExpr();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseAndExpr():
  //
  // and-expr ::= relational-expr ( "&&" relational-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseAndExpr() throws SyntaxError {
    parseRelationalExpr();
    while(currentToken.kind == Token.AND) {
      accept(Token.AND);
      parseRelationalExpr();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseRelationalExpr():
  //
  // relational-expr ::= add-expr ( ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) add-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseRelationalExpr() throws SyntaxError {
    parseAddExpr();
    while(isRelationalToken(currentToken.kind)) {
      acceptIt();
      parseAddExpr();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseAddExpr():
  //
  // add-expr ::= mult-expr ( ( "+" | "-" ) mult-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseAddExpr() throws SyntaxError {
    parseMultExpr();
    while(isAddToken(currentToken.kind)) {
      acceptIt();
      parseMultExpr();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseMultExpr():
  //
  // mult-expr ::= unary-expr ( ( "*" | "/" ) unary-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseMultExpr() throws SyntaxError {
    parseUnaryExpr();
    while(isMultToken(currentToken.kind)) {
      acceptIt();
      parseUnaryExpr();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseUnaryExpr():
  //
  // unary-expr ::= ( "+" | "-" | "!" )* primary-expr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseUnaryExpr() throws SyntaxError {
    while(isUnaryToken(currentToken.kind)) {
      acceptIt();
    }
    parsePrimaryExpr();
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parsePrimaryExpr():
  //
  // primary-expr ::= ID arglist?
  //                | ID "[" expr "]"
  //                | "(" expr ")"
  //                | INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parsePrimaryExpr() throws SyntaxError {
    if(currentToken.kind == Token.ID) {
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN) {
        parseArgList();
      }
      else if(currentToken.kind == Token.LEFTBRACKET) {
        accept(Token.LEFTBRACKET);
        parseExpr();
        accept(Token.RIGHTBRACKET);
      }
    }
    else if(currentToken.kind == Token.LEFTPAREN) {
      accept(Token.LEFTPAREN);
      parseExpr();
      accept(Token.RIGHTPAREN);
    }
    else if(currentToken.kind == Token.INTLITERAL) {
      accept(Token.INTLITERAL);
    }
    else if(currentToken.kind == Token.BOOLLITERAL) {
      accept(Token.BOOLLITERAL);
    }
    else if(currentToken.kind == Token.FLOATLITERAL) {
      accept(Token.FLOATLITERAL);
    }
    else if(currentToken.kind == Token.STRINGLITERAL) {
      accept(Token.STRINGLITERAL);
    }
    else {
      syntaxError("\"%\" not expected here, expected : ID | \"(\" | TypeSpecifier",
          currentToken.GetLexeme());
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseAsgnExpr():
  //
  // asgnexpr ::= ID "=" expr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseAsgnExpr() throws SyntaxError {
    accept(Token.ID);
    accept(Token.ASSIGN);
    parseExpr();
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgList():
  //
  // arglist ::= "(" args? ")"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    if(isFirstExpr(currentToken.kind)) {
      parseArgs();
    }
    accept(Token.RIGHTPAREN);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgs():
  //
  // args ::= arg ( "," arg )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseArgs() throws SyntaxError {
    parseArg();
    while(currentToken.kind == Token.COMMA) {
      accept(Token.COMMA);
      parseArg();
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArg():
  //
  // arg ::= expr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseArg() throws SyntaxError {
    parseExpr();
  }

}
