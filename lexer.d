import std.algorithm, std.string, std.array, std.range, std.ascii, std.typecons, std.conv;

unittest {
  string code = "
    if x = 3 then
      print(x);
    endif
  ";

  import std.stdio;
  assert(tokenize(code) == ["\n1", "if", "x", "=", "3", "then", "\n1", "print", "(", "x", ")", ";", "\n1", "endif"]);
}

auto tokenize(string s) @safe {
  auto app = appender!(string[]);

  static immutable funcsAlpha = [&matchIdent, &matchKeyword];
  static immutable funcsOther = [&matchInt, &matchFloat, &matchString, &matchChar, &matchOperator];

  string[] matches;
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
          app.put("\n" ~ white[1].to!string);
          lineNum += white[1];
        }
      }

      auto com = s.matchComment;
      if (com[0].length) {
        s = s[com[0].length..$];
        somethingChanged = true;
        if (!s.length) break outer;
        
        if (com[1]) {
          app.put("\n" ~ com[1].to!string);
          lineNum += com[1];
        }
      }
    }

    auto funcs = s[0].isAlpha ? funcsAlpha : funcsOther;
    matches.length = 0;

    foreach (f; funcs) {
      matches ~= f(s);
    }

    string longest = matches.fold!((a,b) => a.length > b.length ? a : b);
    if (!longest.length) {
      string errorPortion;
      auto newLineIdx = s.countUntil("\n");
      if (newLineIdx == -1) errorPortion = s;
      else                  errorPortion = s[0..newLineIdx]; 

      throw new Exception("Lexing error on line " ~ lineNum.to!string ~ ": " ~ errorPortion);
    }

    app.put(longest);
    s = s[longest.length..$];
  }

  return app.data;
}

string matchIdent(string s) @safe {
  int state = 0;

  foreach (i, c; s) {
    if (state == 0) {
      if (!c.isAlpha) return s[0..i];
      state = 1;
    }
    else { //state == 1
      if (!c.isAlphaNum) return s[0..i];
    }
  }

  return s;
}

string matchInt(string s) @safe {
  bool atLeastOneNum = false;
  if (!s.length) return "";

  if (s[0].isDigit) atLeastOneNum = true;
  else if (s[0] != '+' && s[0] != '-') return "";

  foreach (i, c; s[1-atLeastOneNum..$]) {
    if (!c.isDigit) {
      if (atLeastOneNum) return s[0..i];
      else return "";
    }
  }

  if (atLeastOneNum) return s;
  else return "";
}

string matchFloat(string s) @safe {
  string intPart = matchInt(s);
  if (!intPart.length) return "";

  if (s.length - intPart.length < 2 || s[intPart.length] != '.' || !s[intPart.length+1].isDigit) return "";

  foreach (i, c; s[intPart.length+2..$]) {
    if (!c.isDigit) return s[0..i];
  }

  return s;
}

string matchString(string s) @safe {
  if (s.length < 2 || s[0] != '"') return "";

  foreach (i, c; s[1..$]) {
    if (c == '"') return s[0..i+1];
    else if (c == '\n') return "";
  }

  return "";
}

string matchChar(string s) @safe {
  if (s.length < 3 || s[0] != '\'') return "";

  if (s.length >= 4 && s[1] == '\\' && s[3] == '\'') {
    if (s[2] == '\n') return "";
    else              return s[0..4];
  }
  else if (s[2] == '\'') {
    if (s[1] == '\n' || s[1] == '\'') return "";
    else              return s[0..3];  
  }

  return "";
}

Tuple!(string, int) matchOneLineComment(string s) @safe {
  if (s.length < 2 || !s.startsWith("//")) return tuple("", 0);

  foreach (i, c; s[1..$]) {
    if (c == '\n') return tuple(s[0..i+1], 1);
  }

  return tuple(s, 0);
}

Tuple!(string, int) matchMultilineComment(string s) @safe {
  if (s.length < 2 || !s.startsWith("/*")) return tuple("", 0);

  int newlines = 0;

  foreach (i; 2..s.length-1) {
    if (s[i] == '*' && s[i+1] == '/') return tuple(s[0..i+2], newlines);
    else if (s[i] == '\n') newlines++;
  }

  return tuple("", 0);
}

Tuple!(string, int) matchComment(string s) @safe {
  auto oneLine = matchOneLineComment(s);
  if (!oneLine.length) return matchMultilineComment(s);
  else return oneLine;
}

string matchKeyword(string s) @safe {
  static immutable string[] keywords = [
    "if", "then", "else", "elif", "endif",
    "while", "do", "endwhile",
    "for", "endfor",
    "return",
    "define", "func", "as", "endfunc",
    "or", "and", "not",
    "div", "mod",
    "true", "false", "null"
  ].sort!"a.length > b.length".array;

  foreach (word; keywords) {
    if (s.startsWith(word)) return s[0..word.length];
  }

  return "";
}

string matchOperator(string s) @safe {
  static immutable string[] ops = [
    "(", ")", ":=", ";",
    "+", "-", "*", "/", "|", "&", "^", "~",
    ":", ",",
    "=", "!=", ">", "<", ">=", "<="
  ].sort!"a.length > b.length".array;

  foreach (op; ops) {
    if (s.startsWith(op)) return s[0..op.length];
  }

  return "";  
}

Tuple!(string, int) matchWhitespace(string s) @safe {
  int newlines = 0;

  foreach (i, c; s) {
    if (!c.isWhite) return tuple(s[0..i], newlines);
    else if (c == '\n') newlines++;
  }

  return tuple(s, newlines);
}
