package MiniC.Scanner;

import MiniC.Scanner.SourceFile;
import MiniC.Scanner.Token;

public final class Scanner {

  private SourceFile sourceFile;

  private char currentChar;
  private boolean verbose;
  private StringBuffer currentLexeme;
  private boolean currentlyScanningToken;
  private int currentLineNr;
  private int currentColNr;

  private boolean currentlyLookingAhead;
  private StringBuffer LookAheadBuffer;

  private boolean escapeSeq;
  private boolean isLastCharStar;

  private boolean isDigit(char c) {
    return (c >= '0' && c <= '9');
  }

  private boolean isLetter(char c) {
    return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_'));
  }


///////////////////////////////////////////////////////////////////////////////

  public Scanner(SourceFile source) {
    sourceFile = source;
    currentChar = sourceFile.readChar();
    verbose = false;
    currentLineNr = 1;
    currentColNr= 1;
    LookAheadBuffer = new StringBuffer("");
    escapeSeq = false;
  }

  public void enableDebugging() {
    verbose = true;
  }

  // takeIt appends the current character to the current token, and gets
  // the next character from the source program (or the to-be-implemented
  // "untake" buffer in case of look-ahead characters that got 'pushed back'
  // into the input stream).

  private void takeIt() {
    if(currentlyLookingAhead) {
      LookAheadBuffer.append(currentChar);
      currentChar = sourceFile.readChar();
    }
    else {
      if(currentChar == '\n') {
        currentLineNr++;
        currentColNr = 1;
      }
      else {
        currentColNr++;
      }
      if(currentlyScanningToken) {
        currentLexeme.append(currentChar);
      }
      if(LookAheadBuffer.length() == 0) {
        currentChar = sourceFile.readChar();
      }
      else {
        currentChar = LookAheadBuffer.charAt(0);
        LookAheadBuffer.deleteCharAt(0);
      }
    }
  }

  private int scanToken() {

    switch (currentChar) {

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
      takeIt();
      while (isDigit(currentChar)) {
        takeIt();
      }
      // Note: code for floating point literals is missing here...
      // 12E~~
      if(currentChar == 'E' || currentChar == 'e') {
        currentlyLookingAhead = true;
        takeIt();
        // 12E+~~
        if(currentChar == '+' || currentChar == '-') {
          takeIt();
        }
        // 12E2~~
        if(isDigit(currentChar)) {
          currentLexeme.append(LookAheadBuffer);
          currentColNr = currentColNr + LookAheadBuffer.length();
          LookAheadBuffer = new StringBuffer("");
          currentlyLookingAhead = false;
          takeIt();
          while(isDigit(currentChar)) {
            takeIt();
          }
          return Token.FLOATLITERAL;
        }
        // 12EAB~~
        else {
          LookAheadBuffer.append(currentChar);
          currentChar = LookAheadBuffer.charAt(0);
          LookAheadBuffer.deleteCharAt(0);
          currentlyLookingAhead = false;
          return Token.INTLITERAL;
        }
      }
      //12.~~
      else if(currentChar == '.') {
        takeIt();
        // 12.34~
        if(isDigit(currentChar)) {
          while(isDigit(currentChar)) {
            takeIt();
          }
          // 12.34e~~
          if(currentChar == 'E' || currentChar == 'e') {
            currentlyLookingAhead = true;
            takeIt();
            // 12.34e+
            if(currentChar == '+' || currentChar == '-') {
              takeIt();
            }
            //12.34e56
            if(isDigit(currentChar)) {
              currentLexeme.append(LookAheadBuffer);
              currentColNr = currentColNr + LookAheadBuffer.length();
              LookAheadBuffer = new StringBuffer("");
              currentlyLookingAhead = false;
              takeIt();
              while(isDigit(currentChar)) {
                takeIt();
              }
              return Token.FLOATLITERAL;
            }
            else {
              LookAheadBuffer.append(currentChar);
              currentChar = LookAheadBuffer.charAt(0);
              LookAheadBuffer.deleteCharAt(0);
              currentlyLookingAhead = false;
              return Token.FLOATLITERAL;
            }
          }
          else {
            return Token.FLOATLITERAL;
          }
        }
        // 12.e~~
        else if(currentChar == 'E' || currentChar == 'e') {
          currentlyLookingAhead = true;
          takeIt();
          //12.e+
          if(currentChar == '+' || currentChar == '-') {
            takeIt();
          }
          // 12.e34
          if(isDigit(currentChar)) {
            currentLexeme.append(LookAheadBuffer);
            currentColNr = currentColNr + LookAheadBuffer.length();
            LookAheadBuffer = new StringBuffer("");
            currentlyLookingAhead = false;
            takeIt();
            while(isDigit(currentChar)) {
              takeIt();
            }
            return Token.FLOATLITERAL;
          }
          // 12.eAB
          else {
            LookAheadBuffer.append(currentChar);
            currentChar = LookAheadBuffer.charAt(0);
            LookAheadBuffer.deleteCharAt(0);
            currentlyLookingAhead = false;
            return Token.FLOATLITERAL;
          }
        }
        // 12. AB
        else {
          return Token.FLOATLITERAL;
        }

      }
      // 1234
      else {
        return Token.INTLITERAL;
      }
    case '.':
      takeIt();
      // .34~
      if(isDigit(currentChar)) {
        while(isDigit(currentChar)) {
          takeIt();
        }
        // .34e~~
        if(currentChar == 'E' || currentChar == 'e') {
          currentlyLookingAhead = true;
          takeIt();
          // .34e+
          if(currentChar == '+' || currentChar == '-') {
            takeIt();
          }
          //.34e56
          if(isDigit(currentChar)) {
            currentLexeme.append(LookAheadBuffer);
            currentColNr = currentColNr + LookAheadBuffer.length();
            LookAheadBuffer = new StringBuffer("");
            currentlyLookingAhead = false;
            takeIt();
            while(isDigit(currentChar)) {
              takeIt();
            }
            return Token.FLOATLITERAL;
          }
          else {
            LookAheadBuffer.append(currentChar);
            currentChar = LookAheadBuffer.charAt(0);
            LookAheadBuffer.deleteCharAt(0);
            currentlyLookingAhead = false;
            return Token.FLOATLITERAL;
          }
        }
        // .34
        else {
          return Token.FLOATLITERAL;
        }
      }
      // .AB
      else {
        return Token.ERROR;
      }

    case '\u0000': // sourceFile.eot:
      currentLexeme.append('$');
      currentColNr++;
      return Token.EOF;
    // Add code here for the remaining MiniC tokens...
    // punctuations
    case '{':
      takeIt();
      return Token.LEFTBRACE;
    case '}':
      takeIt();
      return Token.RIGHTBRACE;
    case '[':
      takeIt();
      return Token.LEFTBRACKET;
    case ']':
      takeIt();
      return Token.RIGHTBRACKET;
    case '(':
      takeIt();
      return Token.LEFTPAREN;
    case ')':
      takeIt();
      return Token.RIGHTPAREN;
    case ',':
      takeIt();
      return Token.COMMA;
    case ';':
      takeIt();
      return Token.SEMICOLON;
    // operators
    case '=':
      takeIt();
      if(currentChar == '=') {
        takeIt();
        return Token.EQ;
      }
      else {
        return Token.ASSIGN;
      }
    case '|':
      takeIt();
      if(currentChar == '|') {
        takeIt();
        return Token.OR;
      }
      else {
        return Token.ERROR;
      }
    case '&':
      takeIt();
      if(currentChar == '&') {
        takeIt();
        return Token.AND;
      }
      else {
        return Token.ERROR;
      }
    case '!':
      takeIt();
      if(currentChar == '=') {
        takeIt();
        return Token.NOTEQ;
      }
      else {
        return Token.NOT;
      }
    case '<':
      takeIt();
      if(currentChar == '=') {
        takeIt();
        return Token.LESSEQ;
      }
      else {
        return Token.LESS;
      }
    case '>':
      takeIt();
      if(currentChar == '=') {
        takeIt();
        return Token.GREATEREQ;
      }
      else {
        return Token.GREATER;
      }
    case '+':
      takeIt();
      return Token.PLUS;
    case '-':
      takeIt();
      return Token.MINUS;
    case '*':
      takeIt();
      return Token.TIMES;

    case '\"':
      takeIt();
      currentlyLookingAhead = true;
      while(currentChar != '\"') {
        if(escapeSeq && currentChar != 'n') {
          System.out.println("ERROR: Illegal escape sequences in string!");
        }
        if(currentChar == '\n') {
          System.out.println("ERROR: Un-terminated string!");
          currentLexeme.append(LookAheadBuffer);
          currentColNr = currentColNr + LookAheadBuffer.length();
          LookAheadBuffer = new StringBuffer("");
          currentlyLookingAhead = false;
          currentLexeme.deleteCharAt(0);
          return Token.STRINGLITERAL;
        }
        if(currentChar == '\\') {
          escapeSeq = true;
        }
        else {
          escapeSeq = false;
        }
        takeIt();
      }
      takeIt();
      currentLexeme.append(LookAheadBuffer);
      currentColNr = currentColNr + LookAheadBuffer.length();
      LookAheadBuffer = new StringBuffer("");
      currentlyLookingAhead = false;
      currentLexeme.deleteCharAt(currentLexeme.length()-1);
      currentLexeme.deleteCharAt(0);
      return Token.STRINGLITERAL;

    case '/':
      takeIt();
      // End-of-line comment "// ..."
      if(currentChar == '/') {
        takeIt();
        currentlyScanningToken = false;
        while(currentChar != '\n') {
          takeIt();
        }
        takeIt();
        currentLexeme = new StringBuffer("");
        currentlyScanningToken = true;
        return -1;
      }
      // C-Style comment "/* .. */"
      else if(currentChar == '*') {
        takeIt();
        currentlyScanningToken = false;
        isLastCharStar = false;
        while(true) {
          if(currentChar == '\u0000') {
            System.out.println("ERROR: Un-terminated comment!");
            currentLexeme = new StringBuffer("$");
            currentColNr++;
            return Token.EOF;
          }
          if(isLastCharStar && (currentChar == '/')) {
            takeIt();
            currentLexeme = new StringBuffer("");
            currentlyScanningToken = true;
            return -1;
          }
          else {
            if(currentChar == '*') {
              isLastCharStar = true;
            }
            else {
              isLastCharStar = false;
            }
            takeIt();
          }
        }
      }
      else {
        return Token.DIV;
      }

    default:
      if(currentChar == 't') {
        currentlyLookingAhead = true;
        takeIt();
        if(currentChar == 'r') {
          takeIt();
          if(currentChar == 'u') {
            takeIt();
            if(currentChar == 'e') {
              takeIt();
              currentLexeme.append(LookAheadBuffer);
              currentColNr = currentColNr + LookAheadBuffer.length();
              LookAheadBuffer = new StringBuffer("");
              currentlyLookingAhead = false;
              return Token.BOOLLITERAL;
            }
            else {
              LookAheadBuffer.append(currentChar);
              currentChar = LookAheadBuffer.charAt(0);
              LookAheadBuffer.deleteCharAt(0);
              currentlyLookingAhead = false;
            }
          }
          else {
            LookAheadBuffer.append(currentChar);
            currentChar = LookAheadBuffer.charAt(0);
            LookAheadBuffer.deleteCharAt(0);
            currentlyLookingAhead = false;
          }
        }
        else {
          LookAheadBuffer.append(currentChar);
          currentChar = LookAheadBuffer.charAt(0);
          LookAheadBuffer.deleteCharAt(0);
          currentlyLookingAhead = false;
        }
      }
      else if(currentChar == 'f') {
        currentlyLookingAhead = true;
        takeIt();
        if(currentChar == 'a') {
          takeIt();
          if(currentChar == 'l') {
            takeIt();
            if(currentChar == 's') {
              takeIt();
              if(currentChar == 'e') {
                takeIt();
                currentLexeme.append(LookAheadBuffer);
                currentColNr = currentColNr + LookAheadBuffer.length();
                LookAheadBuffer = new StringBuffer("");
                currentlyLookingAhead = false;
                return Token.BOOLLITERAL;
              }
              else {
                LookAheadBuffer.append(currentChar);
                currentChar = LookAheadBuffer.charAt(0);
                LookAheadBuffer.deleteCharAt(0);
                currentlyLookingAhead = false;
              }
            }
            else {
              LookAheadBuffer.append(currentChar);
              currentChar = LookAheadBuffer.charAt(0);
              LookAheadBuffer.deleteCharAt(0);
              currentlyLookingAhead = false;
            }
          }
          else {
            LookAheadBuffer.append(currentChar);
            currentChar = LookAheadBuffer.charAt(0);
            LookAheadBuffer.deleteCharAt(0);
            currentlyLookingAhead = false;
          }
        }
        else {
          LookAheadBuffer.append(currentChar);
          currentChar = LookAheadBuffer.charAt(0);
          LookAheadBuffer.deleteCharAt(0);
          currentlyLookingAhead = false;
        }
      }
      if(isLetter(currentChar)) {
        takeIt();
        while(isLetter(currentChar) || isDigit(currentChar)) {
          takeIt();
        }
        return Token.ID;
      }
      else {
        takeIt();
        return Token.ERROR;
      }
    }
  }

  public Token scan() {
    Token currentToken;
    SourcePos pos;
    int kind;

    do {
      currentlyScanningToken = false;
      while (currentChar == ' '
             || currentChar == '\f'
             || currentChar == '\n'
             || currentChar == '\r'
             || currentChar == '\t')
      {
        takeIt();
      }

      currentlyScanningToken = true;
      currentlyLookingAhead = false;
      currentLexeme = new StringBuffer("");
      pos = new SourcePos();
      // Note: currentLineNr and currentColNr are not maintained yet!
      pos.StartLine = currentLineNr;
      pos.EndLine = currentLineNr;
      pos.StartCol = currentColNr;
      kind = scanToken();
    } while(kind == -1);
    currentToken = new Token(kind, currentLexeme.toString(), pos);
    pos.EndCol = currentColNr - 1;
    if (verbose)
      currentToken.print();
    return currentToken;
  }

}
