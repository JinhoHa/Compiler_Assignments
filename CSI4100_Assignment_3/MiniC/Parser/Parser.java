package MiniC.Parser;

import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePos previousTokenPosition;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.GetSourcePos();
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
    previousTokenPosition = currentToken.GetSourcePos();
    currentToken = scanner.scan();
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.
  void start(SourcePos position) {
    position.StartCol = currentToken.GetSourcePos().StartCol;
    position.StartLine = currentToken.GetSourcePos().StartLine;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.
  void finish(SourcePos position) {
    position.EndCol = previousTokenPosition.EndCol;
    position.EndLine = previousTokenPosition.EndLine;
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if(token == Token.VOID ||
        token == Token.INT  ||
        token == Token.BOOL ||
        token == Token.FLOAT) {
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
  // parseArrayIndexDecl (Type T):
  //
  // Take [INTLITERAL] and generate an ArrayType
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
    IntLiteral L;
    IntExpr IE;
    accept(Token.LEFTBRACKET);
    SourcePos pos = currentToken.GetSourcePos();
    L = new IntLiteral(currentToken.GetLexeme(), pos);
    accept(Token.INTLITERAL);
    accept(Token.RIGHTBRACKET);
    IE = new IntExpr (L, pos);
    return new ArrayType (T, IE, previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Program parse() {

    Program ProgramAST = null;

    previousTokenPosition = new SourcePos();
    previousTokenPosition.StartLine = 0;
    previousTokenPosition.StartCol = 0;
    previousTokenPosition.EndLine = 0;
    previousTokenPosition.EndCol = 0;

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      ProgramAST = parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) { return null; }
    return ProgramAST;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  // parseProgDecls: recursive helper function to facilitate AST construction.
  public Decl parseProgDecls () throws SyntaxError {
    if (! isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    if(currentToken.kind == Token.LEFTPAREN) {
      Decl newD = parseFunPart(T, Ident, pos);
      return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
    } else {
      DeclSequence Vars = parseVarPart(T, Ident);
      DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
      Decl RemainderDecls = parseProgDecls();
      VarsTail.SetRightSubtree (RemainderDecls);
      return Vars;
    }
  }

  public Program parseProgram() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Decl D = parseProgDecls();
    finish(pos);
    Program P = new Program (D, pos);
    return P;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    Decl PDecl = parseParamsList(); // can also be empty...
    accept(Token.RIGHTPAREN);
    CompoundStmt CStmt = parseCompoundStmt();
    finish(pos);
    return new FunDecl (T, Ident, PDecl, CStmt, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParamsList() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyFormalParamDecl(previousTokenPosition);
    }
    Decl PDecl = parseParameterDecl();
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
    }
    return new FormalParamDeclSequence (PDecl,
        parseParamsList(), previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParameterDecl():
  //
  // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParameterDecl() throws SyntaxError {
    Type T = null;
    Decl D = null;

    SourcePos pos = new SourcePos();
    start(pos);
    if (isTypeSpecifier(currentToken.kind)) {
      T = parseTypeSpecifier();
    } else {
      syntaxError("Type specifier instead of % expected",
          Token.spell(currentToken.kind));
    }
    D = parseDeclarator(T, pos);
    return D;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseDeclarator():
  //
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
    ID Ident = parseID();
    if (currentToken.kind == Token.LEFTBRACKET) {
      ArrayType ArrT = parseArrayIndexDecl(T);
      finish(pos);
      return new FormalParamDecl (ArrT, Ident, pos);
    }
    finish(pos);
    return new FormalParamDecl (T, Ident, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
    Type theType = T;
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      // You can use the following code after you have implemented
      // parseInitializer():
      // E = parseInitializer();
      E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    // You can use the following code after implementatin of parseInitDecl():
    /*
       if (currentToken.kind == Token.COMMA) {
       acceptIt();
       Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
       } else {
       Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
       previousTokenPosition);
       }
     */
     if (currentToken.kind == Token.COMMA) {
       acceptIt();
       Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
     }
     else {
       Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
       previousTokenPosition);
     }
    accept (Token.SEMICOLON);
    return Seq;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseInitializer():
  //
  // initializer ::= expr
  //               | "{" expr ( "," expr )* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseInitializer() throws SyntaxError {
    // case ExprSequence
    if(currentToken.kind == Token.LEFTBRACE) {
      acceptIt();
      ExprSequence Seq = null;
      Expr E = parseExpr();
      if(currentToken.kind == Token.COMMA) {
        acceptIt();
        Seq = new ExprSequence(E, parseExprSeq(), previousTokenPosition);
      }
      else {
        Seq = new ExprSequence(E, new EmptyExpr(previousTokenPosition), previousTokenPosition);
      }
      accept(Token.RIGHTBRACE);
      return Seq;
    }
    // case Expr
    else {
      Expr E = parseExpr();
      return E;
    }
  }

  public Expr parseExprSeq() throws SyntaxError {
    ExprSequence Seq = null;
    Expr E = parseExpr();
    if(currentToken.kind == Token.COMMA) {
      acceptIt();
      Seq = new ExprSequence(E, parseExprSeq(), previousTokenPosition);
    }
    else {
      Seq = new ExprSequence(E, new EmptyExpr(previousTokenPosition), previousTokenPosition);
    }
    return Seq;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseInitDecl():
  //
  // init_decl ::= declarator ( "=" initializer )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  // return DeclSequence
  public Decl parseInitDecl(Type T) throws SyntaxError {
    Type theType = T;
    ID Ident = parseID();
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);

    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
    }
    else {
      Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
      previousTokenPosition);
    }

    return Seq;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseExpr():
  //
  // expr ::= and-expr ( "||" and-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr E = null;
    Expr E1 = parseAndExpr();
    E = E1;
    while(currentToken.kind == Token.OR) {
      E1 = E;
      Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
      acceptIt();
      Expr E2 = parseAndExpr();
      finish(pos);
      E = new BinaryExpr(E1, opAST, E2, pos);
    }
    return E;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseAndExpr():
  //
  // and-expr ::= relational-expr ( "&&" relational-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseAndExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr E = null;
    Expr E1 = parseRelationalExpr();
    E = E1;
    while(currentToken.kind == Token.AND) {
      E1 = E;
      Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
      acceptIt();
      Expr E2 = parseRelationalExpr();
      finish(pos);
      E = new BinaryExpr(E1, opAST, E2, pos);
    }
    return E;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseRelationalExpr():
  //
  // relational-expr ::= add-expr ( ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) add-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseRelationalExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr E = null;
    Expr E1 = parseAddExpr();
    E = E1;
    while(currentToken.kind == Token.EQ ||
      currentToken.kind == Token.NOTEQ  ||
      currentToken.kind == Token.LESSEQ ||
      currentToken.kind == Token.LESS ||
      currentToken.kind == Token.GREATEREQ ||
      currentToken.kind == Token.GREATER) {
      E1 = E;
      Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
      acceptIt();
      Expr E2 = parseAddExpr();
      finish(pos);
      E = new BinaryExpr(E1, opAST, E2, pos);
    }
    return E;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseAddExpr():
  //
  // add-expr ::= mult-expr ( ( "+" | "-" ) mult-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseAddExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr E = null;
    Expr E1 = parseMultExpr();
    E = E1;
    while(currentToken.kind == Token.PLUS ||
      currentToken.kind == Token.MINUS) {
      E1 = E;
      Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
      acceptIt();
      Expr E2 = parseMultExpr();
      finish(pos);
      E = new BinaryExpr(E1, opAST, E2, pos);
    }
    return E;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseMultExpr():
  //
  // mult-expr ::= unary-expr ( ( "*" | "/" ) unary-expr )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseMultExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr E = null;
    Expr E1 = parseUnaryExpr();
    E = E1;
    while(currentToken.kind == Token.TIMES ||
      currentToken.kind == Token.DIV) {
      E1 = E;
      Operator opAST = new Operator(currentToken.GetLexeme(), previousTokenPosition);
      acceptIt();
      Expr E2 = parseUnaryExpr();
      finish(pos);
      E = new BinaryExpr(E1, opAST, E2, pos);
    }
    return E;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseUnaryExpr():
  //
  // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseUnaryExpr() throws SyntaxError {
    while (currentToken.kind == Token.PLUS ||
        currentToken.kind == Token.MINUS ||
        currentToken.kind == Token.NOT) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      return new UnaryExpr (opAST, parseUnaryExpr(), previousTokenPosition);
    }
    return parsePrimaryExpr();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parsePrimaryExpr():
  //
  // PrimaryExpr ::= ID arglist?
  //              |  ID "[" expr "]"
  //              |  "(" expr ")"
  //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parsePrimaryExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr retExpr = null;

    // your code goes here...
    if(currentToken.kind == Token.INTLITERAL) {
      Expr E = new IntExpr(new IntLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
      acceptIt();
      return E;
    }
    else if(currentToken.kind == Token.BOOLLITERAL) {
      Expr E = new BoolExpr(new BoolLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
      acceptIt();
      return E;
    }
    else if(currentToken.kind == Token.FLOATLITERAL) {
      Expr E = new FloatExpr(new FloatLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
      acceptIt();
      return E;
    }
    else if(currentToken.kind == Token.STRINGLITERAL) {
      Expr E = new StringExpr(new StringLiteral(currentToken.GetLexeme(), previousTokenPosition), previousTokenPosition);
      acceptIt();
      return E;
    }
    else if(currentToken.kind == Token.LEFTPAREN) {
      accept(Token.LEFTPAREN);
      retExpr = parseExpr();
      accept(Token.RIGHTPAREN);
    }
    else if(currentToken.kind == Token.ID) {
      ID Ident = parseID();
      retExpr = new VarExpr(Ident, previousTokenPosition);
      if(currentToken.kind == Token.LEFTPAREN) {
        Expr paramE = parseArgList();
        finish(pos);
        retExpr = new CallExpr(Ident, paramE, pos);
      }
      else if(currentToken.kind == Token.LEFTBRACKET) {
        Expr VE = new VarExpr(Ident, previousTokenPosition);
        accept(Token.LEFTBRACKET);
        Expr E = parseExpr();
        accept(Token.RIGHTBRACKET);
        finish(pos);
        retExpr = new ArrayExpr(VE, E, pos);
      }
    }
    else {
      syntaxError("\"%\" not expected here",
        currentToken.GetLexeme());
    }


    return retExpr;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDef* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseCompoundDecls () throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    DeclSequence Vars = parseVarPart(T, Ident);
    DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    Decl RemainderDecls = parseCompoundDecls();
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;
  }

  public Stmt parseCompoundStmts () throws SyntaxError {
    if (! (currentToken.kind == Token.LEFTBRACE ||
          currentToken.kind == Token.IF ||
          currentToken.kind == Token.WHILE ||
          currentToken.kind == Token.FOR ||
          currentToken.kind == Token.RETURN ||
          currentToken.kind == Token.ID)
       ) {
      return new EmptyStmt(previousTokenPosition);
    }
    Stmt S = null;
    // You can use the following code after implementation of parseStmt():
    //S = parseStmt();
    S = parseStmt();
    return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
  }

  public CompoundStmt parseCompoundStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.LEFTBRACE);
    Decl D = parseCompoundDecls();
    Stmt S = parseCompoundStmts();
    accept(Token.RIGHTBRACE);
    finish(pos);
    if ( (D.getClass() == EmptyDecl.class) &&
        (S.getClass() == EmptyStmt.class)) {
      return new EmptyCompoundStmt (previousTokenPosition);
    } else {
      return new CompoundStmt (D, S, pos);
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseStmt():
  //
  // Stmt ::= CompoundStmt
  //        | if-stmt
  //        | while-stmt
  //        | for-stmt
  //        | return-stmt
  //        | ID ( ( "=" expr ) | ( "[" expr "]" "=" expr ) | ( arglist ) ) ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseStmt() throws SyntaxError {
    Stmt S = null;
    SourcePos pos = new SourcePos();
    start(pos);
    if(currentToken.kind == Token.LEFTBRACE) {
      return parseCompoundStmt();
    }
    else if(currentToken.kind == Token.IF) {
      return parseIfStmt();
    }
    else if(currentToken.kind == Token.WHILE) {
      return parseWhileStmt();
    }
    else if(currentToken.kind == Token.FOR) {
      return parseForStmt();
    }
    else if(currentToken.kind == Token.RETURN) {
      return parseReturnStmt();
    }
    else if(currentToken.kind == Token.ID) {
      SourcePos IdPos = new SourcePos();
      start(IdPos);
      ID Ident = parseID();
      if(currentToken.kind == Token.LEFTPAREN) {
        return parseCallStmt(Ident, pos);
      }
      else if(currentToken.kind == Token.LEFTBRACKET ||
      currentToken.kind == Token.ASSIGN) {
        Expr VE = new VarExpr(Ident, previousTokenPosition);
        Expr LE = VE;
        if(currentToken.kind == Token.LEFTBRACKET) {
          acceptIt();
          Expr E = parseExpr();
          accept(Token.RIGHTBRACKET);
          finish(IdPos);
          LE = new ArrayExpr(VE, E, IdPos);
        }
        accept(Token.ASSIGN);
        Expr RE = parseExpr();
        finish(pos);
        accept(Token.SEMICOLON);
        return new AssignStmt(LE, RE, pos);
      }
    }
    syntaxError("\"%\" not expected here",
      currentToken.GetLexeme());
    return null;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseIfStmt():
  //
  // if-stmt ::= if "(" expr ")" stmt ( else stmt )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseIfStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    Expr eE = parseExpr();
    accept(Token.RIGHTPAREN);
    Stmt thenS = parseStmt();
    if(currentToken.kind == Token.ELSE) {
      acceptIt();
      Stmt elseS = parseStmt();
      finish(pos);
      return new IfStmt(eE, thenS, elseS, pos);
    }
    finish(pos);
    return new IfStmt(eE, thenS, pos);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseWhileStmt():
  //
  // while-stmt ::= while "(" expr ")" stmt
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseWhileStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    Expr E = parseExpr();
    accept(Token.RIGHTPAREN);
    Stmt S = parseStmt();
    finish(pos);
    return new WhileStmt(E, S, pos);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseForStmt():
  //
  // for-stmt ::= for "(" asgnexpr? ";" expr? ";" asgnexpr? ")" stmt
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseForStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    Expr E1 = new EmptyExpr(previousTokenPosition);
    if(currentToken.kind == Token.ID) {
      E1 = parseAsgnExpr();
    }
    accept(Token.SEMICOLON);
    Expr E2 = new EmptyExpr(previousTokenPosition);
    if (isFirstExpr(currentToken.kind)) {
      E2 = parseExpr();
    }
    accept(Token.SEMICOLON);
    Expr E3 = new EmptyExpr(previousTokenPosition);
    if(currentToken.kind == Token.ID) {
      E3 = parseAsgnExpr();
    }
    accept(Token.RIGHTPAREN);
    Stmt S = parseStmt();

    finish(pos);
    return new ForStmt(E1, E2, E3, S, pos);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseReturnStmt():
  //
  // returnStmt ::= return expr? ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseReturnStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.RETURN);
    Expr E = new EmptyExpr(previousTokenPosition);
    if (isFirstExpr(currentToken.kind)) {
      E = parseExpr();
    }
    finish(pos);
    accept(Token.SEMICOLON);

    return new ReturnStmt(E, pos);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCallStmt():
  //
  // call-stmt ::= ID arglist ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseCallStmt(ID Ident, SourcePos pos) throws SyntaxError {
    Expr paramE = parseArgList();
    finish(pos);
    Expr callE = new CallExpr(Ident, paramE, pos);
    accept(Token.SEMICOLON);
    return new CallStmt(callE, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgList():
  //
  // ArgList ::= "(" ( arg ( "," arg )* )? ")"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseArgs() throws SyntaxError {
    if (currentToken.kind == Token.RIGHTPAREN) {
      return new  EmptyActualParam (previousTokenPosition);
    }
    Expr Params = null;
    /*
     * You can use the following code after you have implemented parseExpr() aso.:
     *
     *
     Params = new ActualParam (parseExpr(), previousTokenPosition);
     if (currentToken.kind == Token.COMMA) {
     acceptIt();
     }
     */
     Params = new ActualParam(parseExpr(), previousTokenPosition);
     if(currentToken.kind == Token.COMMA) {
       acceptIt();
     }
    return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
  }

  public Expr parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    Expr Params = parseArgs();
    accept(Token.RIGHTPAREN);
    return Params;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseAsgnExpr():
  //
  // asgnexpr ::= ID "=" expr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseAsgnExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    ID Ident = parseID();
    Expr E1 = new VarExpr(Ident, previousTokenPosition);
    accept(Token.ASSIGN);
    Expr E2 = parseExpr();
    finish(pos);
    return new AssignExpr(E1, E2, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseID():
  //
  // ID (terminal)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ID parseID() throws SyntaxError {
    ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
    accept(Token.ID);
    return Ident;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseTypeSpecifier():
  //
  // VOID | INT | FLOAT | BOOL (all terminals)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Type parseTypeSpecifier() throws SyntaxError {
    Type T = null;
    switch (currentToken.kind) {
      case Token.INT:
        T = new IntType(currentToken.GetSourcePos());
        break;
      case Token.FLOAT:
        T = new FloatType(currentToken.GetSourcePos());
        break;
      case Token.BOOL:
        T = new BoolType(currentToken.GetSourcePos());
        break;
      case Token.VOID:
        T = new VoidType(currentToken.GetSourcePos());
        break;
      default:
        syntaxError("Type specifier expected", "");
    }
    acceptIt();
    return T;
  }

}
