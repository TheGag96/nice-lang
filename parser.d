import lexer, std.algorithm, std.typecons, std.string, std.range, std.conv;

void main() @safe {
  import std.stdio;

  writeln("Type in some integer math expressions.");

  bool exit = false;

  Context context = new Context;

  while (!exit) {
    write(">");
    auto line = (() @trusted => readln.strip)();

    TokenRange tokens; 
    try {
      tokens = tokenize(line);
    }
    catch (Exception e) {
      writeln(e.msg);
      continue;
    }

    try {
      auto assign = parseAssignmentStmt(tokens);

      if (assign[0]) {
        assign[0].interpret(context);
      }
      else {
        auto exp = parseExpression(tokens);
        writeln(exp[0].interpret(context));
      }
    }
    catch (Exception e) {
      writeln(e.msg);
    }
  }
}

///////////////////////////
// Some helper functions //
///////////////////////////

void parseError(string msg, int line) @safe {
  throw new Exception(format("Parsing error on line %d: %s", line, msg));
}

void runtimeError(string msg, int line) @safe {
  throw new Exception(format("Runtime error on line %d: %s", line, msg));
}

/////////////////////////////////////////
// Recurisve descent parsing functions //
/////////////////////////////////////////

//Program parse(string s) {
//  auto tokens = tokenize(s);

//  auto result = parseProgram(s)[0];
//  if (!result) parseError("bleh");
//  return result;
//}

//Tuple!(Program, TokenRange) parseProgram(TokenRange tokens) {
//  return parseOuterStmtSeq(tokens);
//}

//Tuple!(Statement[], TokenRange) parseOuterStmtSeq(TokenRange tokens) {
//  Statement[] stmts;

//  auto stmt = parseStatement(tokens);
//  if (!stmt[0]) return tuple(null, tokens);

//  while (stmt[0]) {
//    stmts ~= stmt[0];
//    tokens = stmt[1];
//    stmt   = parseStatement(tokens);    
//  }

//  return tuple(stmts, tokens);  
//}

Tuple!(AssignStatement, TokenRange) parseAssignmentStmt(TokenRange tokens) @safe {
  auto val = parseValue(tokens);
  if (!val[0]) return tuple(cast(AssignStatement)null, tokens);
  int lineNum = tokens.front.lineNum;
  tokens = val[1];

  if (tokens.front != ":=") return tuple(cast(AssignStatement)null, tokens);
  //if (tokens.front != ":=") parseError("Expected ':=' for assignment statement");
  tokens.popFront;

  auto exp = parseExpression(tokens);
  if (!exp[0]) parseError("Expected expression for assignment statement", tokens.front.lineNum);
  tokens = exp[1];

  if (tokens.front != ";") parseError("Expected ';' to end assignment statement", tokens.front.lineNum);
  tokens.popFront;

  return tuple(new AssignStatement(val[0], exp[0], lineNum), tokens);
}

Tuple!(Value, TokenRange) parseValue(TokenRange tokens) @safe {
  Value result;
  auto tok = tokens.front;
  if (tok.type == Token.Type.IDENT) {
    result = new Value(tok, tok.lineNum);
    tokens.popFront;
  }
  return tuple(result, tokens);
}

Tuple!(T, TokenRange) parseBinary(T, U, string[] ops, alias nextFunc)(TokenRange tokens) @safe {
  auto expNext = nextFunc(tokens);
  if (!expNext[0]) return tuple(cast(T)null, tokens);
  int lineNum = tokens.front.lineNum;
  tokens = expNext[1];

  Tuple!(string, U)[] extra;

  while (ops.canFind(tokens.front)) {
    string op = tokens.front;
    tokens.popFront;

    auto extraExp = nextFunc(tokens);
    if (!extraExp[0]) parseError("Invalid binary expression " ~ T.stringof ~ " after " ~ op, lineNum);
    tokens = extraExp[1];

    extra ~= tuple(op, extraExp[0]);
  }

  return tuple(new T(expNext[0], extra, lineNum), tokens);
}


Tuple!(T, TokenRange) parseUnary(T, string[] ops, alias nextFunc)(TokenRange tokens) @safe {
  string op = "";
  int lineNum = tokens.front.lineNum;
  if (ops.canFind(tokens.front)) {
    op = tokens.front;
    tokens.popFront;
  }

  auto expNext = nextFunc(tokens);
  if (!expNext[0]) parseError("Invalid unary expression " ~ T.stringof ~ " after " ~ op, lineNum);
  tokens = expNext[1];

  return tuple(new T(op, expNext[0], lineNum), tokens);
}

alias parseExpression  = parseBinary!(Expression,  Expression2, ["or"], parseExpression2);
alias parseExpression2 = parseBinary!(Expression2, Expression3, ["and"], parseExpression3);
alias parseExpression3 = parseUnary!(Expression3, ["not"], parseExpression4);
alias parseExpression5 = parseBinary!(Expression5, Expression6, ["+", "-", "|"], parseExpression6);
alias parseExpression6 = parseBinary!(Expression6, Expression7, ["*", "/", "div", "mod", "&", "^"], parseExpression7);
alias parseExpression7 = parseUnary!(Expression7, ["-", "~"], parseExpression8);

Tuple!(Expression4, TokenRange) parseExpression4(TokenRange tokens) @safe {
  static immutable ops = ["=", "!=", ">", "<", ">=", "<="];

  auto expNext = parseExpression5(tokens);
  if (!expNext[0]) return tuple(cast(Expression4)null, tokens);
  int lineNum = tokens.front.lineNum;
  tokens = expNext[1];

  Tuple!(string, Expression5) extra;

  //you can only have at most 1 comparison operation here. no more
  if (ops.canFind(tokens.front)) {
    string op = tokens.front;
    tokens.popFront;

    auto extraExp = parseExpression5(tokens);
    if (!extraExp[0]) parseError("Invalid binary expression Expression4 after " ~ op, lineNum);
    tokens = extraExp[1];

    extra = tuple(op, extraExp[0]);
  }

  return tuple(new Expression4(expNext[0], extra, lineNum), tokens);
}

Tuple!(Expression8, TokenRange) parseExpression8(TokenRange tokens) @safe {
  int literal; 
  Value val;
  Expression sub;
  int lineNum = tokens.front.lineNum;
  
  if (tokens.front == "(") {
    tokens.popFront;
    auto loopAround = parseExpression(tokens);
    if (!loopAround[0]) parseError("Invalid expression after (", tokens.front.lineNum);
    sub = loopAround[0];
    tokens = loopAround[1];
    if (tokens.front != ")") parseError("Expected ending ) after expression", tokens.front.lineNum);
    tokens.popFront;

    return tuple(new Expression8(literal, val, sub, lineNum), tokens);
  }

  auto potentialLiteral = parseLiteral(tokens);
  if (potentialLiteral[0] != int.min)  {
    literal = potentialLiteral[0];
    tokens = potentialLiteral[1];
    return tuple(new Expression8(literal, val, sub, lineNum), tokens);
  }

  auto potentialValue = parseValue(tokens);
  if (potentialValue[0])  {
    val = potentialValue[0];
    tokens = potentialValue[1];
    return tuple(new Expression8(literal, val, sub, lineNum), tokens);
  }

  parseError("Expected literal, value, or parenthesized expression", lineNum);
  return tuple(cast(Expression8)null, tokens);
}

Tuple!(int, TokenRange) parseLiteral(TokenRange tokens) @safe {
  if (tokens.front.type == Token.Type.INT) {
    auto result = tokens.front.to!int;
    tokens.popFront;

    return tuple(result, tokens);
  }

  //TODO: Change this to null when Variants are implemented
  return tuple(int.min, tokens);
}

/////////////////
// AST Classes //
/////////////////

class Context {
  int[string][] globalStack;
  int[string] locals;

  int getVar(string name) @safe {
    if (auto local = name in locals) return *local;

    foreach (frame; globalStack) {
      if (auto global = name in frame) return *global;
    }

    //TODO: when making these Variant objects, 
    return int.min;
  }

  void setLocalVar(string name, int val) @safe {
    locals[name] = val;
  }
}

class TreeNode {
  public int lineNum;
}

class Program : TreeNode {
  Statement[] stmts;
}

class Statement : TreeNode {

}

class AssignStatement : Statement {
  Value lhs;
  Expression rhs;  

  this(Value val, Expression exp, int ln) @safe {
    lhs = val;
    rhs = exp;
    lineNum = ln;
  }

  void interpret(Context context) @safe {
    context.setLocalVar(lhs.name, rhs.interpret(context));
  }
}

class CallStatement : Statement {
  
}
class IfStatement : Statement {
  
}
class WhileStatement : Statement {
  
}
class ForStatement : Statement {
  
}
class ReturnStatement : Statement {
  
}
class FuncStatement : Statement {

}
class DefineStatement: Statement {

}

class Value : TreeNode {
  public string name;

  this(string s, int ln) @safe { 
    name = s; 
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    int result = context.getVar(name);

    if (result == int.min) runtimeError("No variable of name " ~ name ~ " has been assigned", lineNum);

    return result;
  }
}

class Expression : TreeNode {
  Expression2 sub;
  Tuple!(string, Expression2)[] extra;

  this(Expression2 s, Tuple!(string, Expression2)[] e, int ln) @safe {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    int result = sub.interpret(context);

    foreach (x; extra) {
      if (result) {
        //short circuit
        result = 1;
        break;
      }

      if (x[0] == "or") {
        if (x[1].interpret(context)) {
          result = 1;
        }
      }
    }

    return result;
  }
}

class Expression2 : TreeNode {
  Expression3 sub;
  Tuple!(string, Expression3)[] extra;

  this(Expression3 s, Tuple!(string, Expression3)[] e, int ln) @safe {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    int result = sub.interpret(context);

    foreach (x; extra) {
      if (!result) break; //short circuit

      if (x[0] == "and") {
        if (x[1].interpret(context)) result = 1;
        else result = 0;
      }
    }

    return result;
  }
}

class Expression3 : TreeNode {
  string op;
  Expression4 sub;

  this(string o, Expression4 s, int ln) @safe {
    op = o;
    sub = s;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    if (op == "not") {
      if (sub.interpret(context)) return 0;
      return 1;
    }

    return sub.interpret(context);
  }
}

class Expression4 : TreeNode {
  Expression5 sub;
  Tuple!(string, Expression5) extra;
  
  this(Expression5 s, Tuple!(string, Expression5) e, int ln) @safe {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    int result = sub.interpret(context);

    if (!extra[1]) return result;
    int rhs = extra[1].interpret(context);

    switch (extra[0]) {
      case "=":
      result = (result == rhs);
      break;

      case "!=":
      result = (result != rhs);
      break;

      case ">":
      result = (result > rhs);
      break;

      case "<":
      result = (result < rhs);
      break;

      case ">=":
      result = (result >= rhs);
      break;

      case "<=":
      result = (result <= rhs);
      break;

      default: assert(0);
    }

    return result;
  }
}

class Expression5 : TreeNode {
  Expression6 sub;
  Tuple!(string, Expression6)[] extra;
  
  this(Expression6 s, Tuple!(string, Expression6)[] e, int ln) @safe {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    int result = sub.interpret(context);

    foreach (x; extra) {
      switch (x[0]) {
        case "+":
        result += x[1].interpret(context);        
        break;

        case "-":
        result -= x[1].interpret(context);        
        break;

        case "|":
        result |= x[1].interpret(context);        
        break;

        default: assert(0);
      }
    }

    return result;
  }
}

class Expression6 : TreeNode {
  Expression7 sub;
  Tuple!(string, Expression7)[] extra;

  this(Expression7 s, Tuple!(string, Expression7)[] e, int ln) @safe {
    sub = s;
    extra = e;
    lineNum = ln;
  }
  
  int interpret(Context context) @safe {
    int result = sub.interpret(context);

    foreach (x; extra) {
      switch (x[0]) {
        case "*":
        result *= x[1].interpret(context);        
        break;

        case "/":
        case "div":
        result /= x[1].interpret(context);        
        break;

        case "mod":
        result %= x[1].interpret(context);        
        break;

        case "&":
        result &= x[1].interpret(context);        
        break;

        case "^":
        result ^= x[1].interpret(context);        
        break;

        default: assert(0);
      }
    }

    return result;
  }
}

class Expression7 : TreeNode {
  string op;
  Expression8 sub;
  
  this(string o, Expression8 s, int ln) @safe {
    op = o;
    sub = s;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    if (op == "-") {
      return -sub.interpret(context);
    }
    else if (op == "~") {
      return ~sub.interpret(context);
    }

    return sub.interpret(context);
  }
}

class Expression8 : TreeNode {
  int literal;
  Value val;
  Expression sub;

  this(int l, Value v, Expression s, int ln) @safe {
    literal = l;
    val = v;
    sub = s;
    lineNum = ln;
  }

  int interpret(Context context) @safe {
    if (sub) return sub.interpret(context);
    else if (val) return val.interpret(context);
    return literal;
  }
}
