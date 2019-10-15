package MiniC.Scanner;

import MiniC.Scanner.SourceFile;
import MiniC.Scanner.Token;
import java.util.LinkedList;
import java.util.Queue;

public final class Scanner {

  private SourceFile sourceFile;

  private char currentChar;
  private boolean verbose;
  private StringBuffer currentLexeme;
  private boolean currentlyScanningToken;
  private int currentLineNr;
  private int currentColNr;

  private boolean currentlyLookingAhead;
  private Queue<Character> LookAheadBuffer;

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
    currentLineNr = -1;
    currentColNr= -1;
    LookAheadBuffer = new LinkedList<Character>();
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
      LookAheadBuffer.offer(currentChar);
    }
    else if (currentlyScanningToken)
    {
      currentLexeme.append(currentChar);
    }
    if (currentlyLookingAhead || LookAheadBuffer.isEmpty()) {
      currentChar = sourceFile.readChar();
    }
    else {
      currentChar = LookAheadBuffer.poll();
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
          while(!LookAheadBuffer.isEmpty()) {
            currentLexeme.append(LookAheadBuffer.poll());
          }
          currentlyLookingAhead = false;
          takeIt();
          while(isDigit(currentChar)) {
            takeIt();
          }
          return Token.FLOATLITERAL;
        }
        // 12EAB~~
        else {
          currentlyLookingAhead = false;
          currentChar = LookAheadBuffer.poll();
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
              while(!LookAheadBuffer.isEmpty()) {
                currentLexeme.append(LookAheadBuffer.poll());
              }
              currentlyLookingAhead = false;
              takeIt();
              while(isDigit(currentChar)) {
                takeIt();
              }
              return Token.FLOATLITERAL;
            }
            else {
              currentlyLookingAhead = false;
              currentChar = LookAheadBuffer.poll();
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
            while(!LookAheadBuffer.isEmpty()) {
              currentLexeme.append(LookAheadBuffer.poll());
            }
            currentlyLookingAhead = false;
            takeIt();
            while(isDigit(currentChar)) {
              takeIt();
            }
            return Token.FLOATLITERAL;
          }
          // 12.eAB
          else {
            currentlyLookingAhead = false;
            currentChar = LookAheadBuffer.poll();
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
    case '\u0000': // sourceFile.eot:
      currentLexeme.append('$');
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
    case '/':
      takeIt();
      return Token.DIV;

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
              while(!LookAheadBuffer.isEmpty()) {
                currentLexeme.append(LookAheadBuffer.poll());
              }
              currentlyLookingAhead = false;
              return Token.BOOLLITERAL;
            }
            else {
              currentlyLookingAhead = false;
              currentChar = LookAheadBuffer.poll();
            }
          }
          else {
            currentlyLookingAhead = false;
            currentChar = LookAheadBuffer.poll();
          }
        }
        else {
          currentlyLookingAhead = false;
          currentChar = LookAheadBuffer.poll();
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
                while(!LookAheadBuffer.isEmpty()) {
                  currentLexeme.append(LookAheadBuffer.poll());
                }
                currentlyLookingAhead = false;
                return Token.BOOLLITERAL;
              }
              else {
                currentlyLookingAhead = false;
                currentChar = LookAheadBuffer.poll();
              }
            }
            else {
              currentlyLookingAhead = false;
              currentChar = LookAheadBuffer.poll();
            }
          }
          else {
            currentlyLookingAhead = false;
            currentChar = LookAheadBuffer.poll();
          }
        }
        else {
          currentlyLookingAhead = false;
          currentChar = LookAheadBuffer.poll();
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
    currentToken = new Token(kind, currentLexeme.toString(), pos);
    pos.EndCol = currentColNr;
    if (verbose)
      currentToken.print();
    return currentToken;
  }

}
