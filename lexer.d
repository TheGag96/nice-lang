import std.algorithm, std.string, std.array, std.range, std.ascii, std.typecons, std.conv;

@safe:

unittest {
  string code = "
    if x = 3 then
      print(x);
    endif
  ";

  assert(tokenize(code).map!(x => x.str).equal(
    ["if", "x", "=", "3", "then", "print", "(", "x", ")", ";", "endif"]
  ));
}

struct Token {
  enum Type {
    EOF = 0,
    IDENT, INT, FLOAT, STRING, CHAR, BOOL, NULL,
    KEYWORD, OP
    //IF, THEN, ELSEIF, ENDIF,
    //WHILE, ENDWHILE, FOR, IN, DO, ENDFOR
    //FUNC, DEFINE, AS, ENDFUNC,
    //LPAREN, RPARENT, DOT, SEMI, ASSIGN,
    //RETURN,
    //OR, AND, NOT,
    //DIV, MOD,
    //TRUE, FALSE, NULL,
    //PLUS, MINUS, TIMES, DIVIDE, BIT_OR, BIT_AND, BIT_NOT, BIT_XOR, 
    //COLON, COMMA, 
    //EQUAL, NOT_EQUAL, GREATER, LESS, GREATER_EQUAL, LESS_EQUAL,
  }

  string str;
  alias str this;
  Type type;
  int lineNum;
}

struct TokenRange {
  Token[] tokens;
  alias tokens this;

  Token front() {
    if (tokens.empty) return Token("", Token.Type.EOF, -1);
    return tokens.front;
  }

  void popFront() {
    if (tokens.length) tokens.popFront;
  }

  bool empty() { return tokens.length == 0; }
}

TokenRange tokenize(string s) {
  auto app = appender!(Token[]);

  static immutable funcsAlpha = [&matchIdent, &matchKeyword]; //@HACK: this order matters for precedence
  static immutable funcsOther = [&matchInt, &matchFloat, &matchString, &matchChar, &matchOperator];

  Token[] matches;
  matches.reserve(funcsOther.length);
  int lineNum = 1;

  outer:
  while (s.length) {
    bool somethingChanged = true;

    while (somethingChanged) {
      somethingChanged = false;

      auto white = s.matchWhitespace;
      if (white[0].length) {
        s = s[white[0].length..$];
        somethingChanged = true;
        if (!s.length) break outer;

        if (white[1]) {
          lineNum += white[1];
        }
      }

      auto com = s.matchComment;
      if (com[0].length) {
        s = s[com[0].length..$];
        somethingChanged = true;
        if (!s.length) break outer;
        
        if (com[1]) {
          lineNum += com[1];
        }
      }
    }

    auto funcs = (s[0].isAlpha || s[0] == '_') ? funcsAlpha : funcsOther;
    matches.length = 0;

    foreach (f; funcs) {
      matches ~= f(s);
    }

    Token longest = matches.fold!((a,b) => a.length > b.length ? a : b);
    if (!longest.length) {
      string errorPortion;
      auto newLineIdx = s.countUntil("\n");
      if (newLineIdx == -1) errorPortion = s;
      else                  errorPortion = s[0..newLineIdx]; 

      throw new Exception("Lexing error on line " ~ lineNum.to!string ~ ": " ~ errorPortion);
    }

    longest.lineNum = lineNum;
    app.put(longest);
    s = s[longest.length..$];
  }

  //app.put(Token("", Token.Type.EOF, lineNum));

  return TokenRange(app.data);
}

Token matchIdent(string s) {
  int state = 0;

  foreach (i, c; s) {
    if (state == 0) {
      if (!(c.isAlpha || c == '_')) return Token(s[0..i], Token.Type.IDENT, 0);
      state = 1;
    }
    else { //state == 1
      if (!(c.isAlphaNum || c == '_')) return Token(s[0..i], Token.Type.IDENT, 0);
    }
  }

  return Token(s, Token.Type.IDENT, 0);
}

Token matchInt(string s) {
  bool atLeastOneNum = false;
  if (!s.length) return Token("", Token.Type.INT, 0);

  if (s[0].isDigit) atLeastOneNum = true;
  else if (s[0] != '+' && s[0] != '-') return Token("", Token.Type.INT, 0);

  foreach (i, c; s[1-atLeastOneNum..$]) {
    if (!c.isDigit) {
      if (atLeastOneNum) return Token(s[0..i], Token.Type.INT, 0);
      else return Token("", Token.Type.INT, 0);
    }
  }

  if (atLeastOneNum) return Token(s, Token.Type.INT, 0);
  else return Token("", Token.Type.INT, 0);
}

Token matchFloat(string s) {
  string intPart = matchInt(s).str;
  if (!intPart.length) return Token("", Token.Type.FLOAT, 0);

  if (s.length - intPart.length < 2 || s[intPart.length] != '.' || !s[intPart.length+1].isDigit) {
    return Token("", Token.Type.FLOAT, 0);
  }

  foreach (i, c; s[intPart.length+2..$]) {
    if (!c.isDigit) return Token(s[0..i+intPart.length+2], Token.Type.FLOAT, 0);
  }

  return Token(s, Token.Type.FLOAT, 0);
}

Token matchString(string s) {
  if (s.length < 2 || s[0] != '"') return Token("", Token.Type.STRING, 0);

  foreach (i, c; s[1..$]) {
    if (c == '"') return Token(s[0..i+2], Token.Type.STRING);
    else if (c == '\n') return Token("", Token.Type.STRING, 0);
  }

  return Token("", Token.Type.STRING, 0);
}

Token matchChar(string s) {
  if (s.length < 3 || s[0] != '\'') return Token("", Token.Type.CHAR, 0);

  if (s.length >= 4 && s[1] == '\\' && s[3] == '\'') {
    if (s[2] == '\n') return Token("", Token.Type.CHAR, 0);
    else              return Token(s[0..4], Token.Type.CHAR, 0);
  }
  else if (s[2] == '\'') {
    if (s[1] == '\n' || s[1] == '\'') return Token("", Token.Type.CHAR, 0);
    else              return Token(s[0..3], Token.Type.CHAR, 0);  
  }

  return Token("", Token.Type.CHAR, 0);
}

Tuple!(string, int) matchOneLineComment(string s) {
  if (s.length < 2 || !s.startsWith("//")) return tuple("", 0);

  foreach (i, c; s[1..$]) {
    if (c == '\n') return tuple(s[0..i+1], 1);
  }

  return tuple(s, 0);
}

Tuple!(string, int) matchMultilineComment(string s) {
  if (s.length < 2 || !s.startsWith("/*")) return tuple("", 0);

  int newlines = 0;

  foreach (i; 2..s.length-1) {
    if (s[i] == '*' && s[i+1] == '/') return tuple(s[0..i+2], newlines);
    else if (s[i] == '\n') newlines++;
  }

  return tuple("", 0);
}

Tuple!(string, int) matchComment(string s) {
  auto oneLine = matchOneLineComment(s);
  if (!oneLine.length) return matchMultilineComment(s);
  else return oneLine;
}

Token matchKeyword(string s) {
  static immutable string[] keywords = [
    "if", "else", "switch", "case",
    "struct", "union", "proc",
    "while", "do", "break", "goto", "continue",
    "for",
    "return",
    "true", "false", "null",
    "try", "catch", "throw",
    "char", "void", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "f32", "f64",
    "bool",
    "string",
    "public", "private",
    "asm",
  ].sort!"a.length > b.length".array;

  foreach (word; keywords) {
    if (s.startsWith(word)) return Token(s[0..word.length], Token.Type.KEYWORD, 0);
  }

  return Token("", Token.Type.KEYWORD, 0);
}

Token matchOperator(string s) {
  static immutable string[] ops = [
    "(", ")", "=", ";", "{", "}", "->",
    "+=", "-=", "*=", "/=", "|=", "&=", "^=", "&&=", "||=",
    "+", "-", "*", "/", "|", "&", "^", "~", "!", "`",
    "||", "&&",
    ":", ",", "..", ".",
    "==", "!=", ">", "<", ">=", "<=",
    "[", "]",
    "=",
  ].sort!"a.length > b.length".array;

  foreach (op; ops) {
    if (s.startsWith(op)) return Token(s[0..op.length], Token.Type.OP, 0);
  }

  return Token("", Token.Type.OP, 0);  
}

Tuple!(string, int) matchWhitespace(string s) {
  int newlines = 0;

  foreach (i, c; s) {
    if (!c.isWhite) return tuple(s[0..i], newlines);
    else if (c == '\n') newlines++;
  }

  return tuple(s, newlines);
}
