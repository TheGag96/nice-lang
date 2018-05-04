import lexer, std.algorithm, std.typecons, std.string, std.range, std.conv;

@safe:

int main(string[] args) {
  import std.stdio, std.file;

  if (args.length != 2) {
    writeln("Usage: parser <program file>");
    return 1;
  }

  auto programText = readText(args[1]);

  try {
    auto program = parse(programText);
    Context context = new Context;
    program.interpret(context);
    writeln("y is ", context.getVar("y"));
  }
  catch (Exception e) {
    writeln(e.msg);
  }

  return 0;
}

void repl() {
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

void parseError(string msg, int line) {
  throw new Exception(format("Parsing error on line %d: %s", line, msg));
}

void runtimeError(string msg, int line) {
  throw new Exception(format("Runtime error on line %d: %s", line, msg));
}

/////////////////////////////////////////
// Recurisve descent parsing functions //
/////////////////////////////////////////

Program parse(string s) {
  auto tokens = tokenize(s);

  auto result = parseProgram(tokens);
  if (!result[0]) parseError("Failed to parse the program!", tokens.front.lineNum);
  tokens = result[1];

  if (tokens.length) {
    parseError("Unexpected text after the program seemingly ended", tokens.front.lineNum);
  }

  return result[0];
}

Tuple!(Program, TokenRange) parseProgram(TokenRange tokens) {
  auto result = parseOuterStmtSeq(tokens);
  int lineNum = tokens.front.lineNum;
  tokens = result[1];

  return tuple(new Program(result[0], lineNum), tokens);
}

Tuple!(StmtSeq, TokenRange) parseOuterStmtSeq(TokenRange tokens) {
  Statement[] stmts;

  int lineNum = tokens.front.lineNum;

  auto stmt = parseStatement(tokens);
  if (!stmt[0]) parseError("Expected statement", tokens.front.lineNum);
  tokens = stmt[1];
  stmts ~= stmt[0];

  while (!tokens.empty) {
    stmt   = parseStatement(tokens);    
    tokens = stmt[1];
    stmts ~= stmt[0];
  }

  return tuple(new StmtSeq(stmts, lineNum), tokens);  
}

bool[string] innerStmtFollowSet = null;
bool[Statement.Type] innerStmtDisallowedSet = null;

Tuple!(StmtSeq, TokenRange) parseInnerStmtSeq(TokenRange tokens) {
  //dangit D, let me use immutable AAs
  if (!innerStmtFollowSet.length) {
    innerStmtFollowSet     = ["elif": true, "else": true, "endif" : true, "endwhile" : true, "endfor" : true, "endfunc" : true];
    innerStmtDisallowedSet = [Statement.Type.FUNC : true, Statement.Type.DEFINE : true];
  }

  Statement[] stmts;

  int lineNum = tokens.front.lineNum;

  while (tokens.front !in innerStmtFollowSet) {
    auto stmt = parseStatement(tokens);
    if (!stmt[0]) parseError("Expected statement", tokens.front.lineNum);
    
    if (stmt[0].type in innerStmtDisallowedSet) {
      parseError("Can't have a statement of type " ~ stmt[0].type.to!string ~ " here", tokens.front.lineNum);
    }
    
    stmts ~= stmt[0];
    tokens = stmt[1];
  }

  return tuple(new StmtSeq(stmts, lineNum), tokens);  
}

Tuple!(Statement, TokenRange) parseStatement(TokenRange tokens) {
  Tuple!(Statement, TokenRange) result;

  switch (tokens.front) {
    case "if":
      result = parseIfStatement(tokens);
    break;
    //case "while":
    //  result = parseIfStatement(tokens);
    //break;
    //case "for":
    //  result = parseIfStatement(tokens);
    //break;
    //case "return":
    //  result = parseIfStatement(tokens);
    //break;
    //case "func":
    //  result = parseIfStatement(tokens);
    //break;
    //case "define":
    //  result = parseIfStatement(tokens);
    //break;
    default:
      if (tokens.front.type == Token.Type.IDENT) {
        result = parseAssignOrCallStatement(tokens);
      }
    break;
  }

  if (!result[0]) parseError("Invalid statement", tokens.front.lineNum);
  return tuple(result[0], result[1]);
}

Tuple!(Statement, TokenRange) parseIfStatement(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;

  if (tokens.front != "if") parseError("Expected 'if' to begin if statement", lineNum);
  tokens.popFront;

  auto cond = parseExpression(tokens);
  if (!cond[0]) parseError("Expected expression after 'if'", tokens.front.lineNum);
  tokens = cond[1];

  if (tokens.front != "then") parseError("Expected 'then' to continue if statement", tokens.front.lineNum);
  tokens.popFront;

  auto mainBody = parseInnerStmtSeq(tokens);
  if (!mainBody[0]) parseError("Expected statements after 'then'", tokens.front.lineNum);
  tokens = mainBody[1];

  Tuple!(Expression, StmtSeq)[] elseIfs;
  while (tokens.front == "elif") {
    tokens.popFront;

    auto elseIfCond = parseExpression(tokens);
    if (!elseIfCond[0]) parseError("Expected expression for elif condition", tokens.front.lineNum);
    tokens = elseIfCond[1];

    if (tokens.front != "then") parseError("Expected 'then' to continue elif", tokens.front.lineNum);
    tokens.popFront;

    auto elseIfBody = parseInnerStmtSeq(tokens);
    if (!elseIfBody[0]) parseError("Expected statements after 'then'", tokens.front.lineNum);
    tokens = elseIfBody[1];

    elseIfs ~= tuple(elseIfCond[0], elseIfBody[0]);
  }

  StmtSeq elseBody;
  if (tokens.front == "else") {
    tokens.popFront;
    
    auto theElse = parseInnerStmtSeq(tokens);
    if (!theElse[0]) parseError("Expected statements after 'else'", tokens.front.lineNum);
    tokens = theElse[1];

    elseBody = theElse[0];
  }

  if (tokens.front != "endif") parseError("Expected 'endif' to end if statement", tokens.front.lineNum);
  tokens.popFront;

  return tuple(
    cast(Statement) (new IfStatement(cond[0], mainBody[0], elseIfs, elseBody, lineNum)),
    tokens
  );
}

Tuple!(Statement, TokenRange) parseAssignOrCallStatement(TokenRange tokens) {
  return parseAssignmentStmt(tokens);
}

Tuple!(Statement, TokenRange) parseAssignmentStmt(TokenRange tokens) {
  auto val = parseValue(tokens);
  if (!val[0]) return tuple(cast(Statement)null, tokens);
  int lineNum = tokens.front.lineNum;
  tokens = val[1];

  if (tokens.front != ":=") return tuple(cast(Statement)null, tokens);
  //if (tokens.front != ":=") parseError("Expected ':=' for assignment statement");
  tokens.popFront;

  auto exp = parseExpression(tokens);
  if (!exp[0]) parseError("Expected expression for assignment statement", tokens.front.lineNum);
  tokens = exp[1];

  if (tokens.front != ";") parseError("Expected ';' to end assignment statement", tokens.front.lineNum);
  tokens.popFront;

  return tuple(
    cast(Statement) (new AssignStatement(val[0], exp[0], lineNum)),
    tokens
  );
}

Tuple!(Value, TokenRange) parseValue(TokenRange tokens) {
  Value result;
  auto tok = tokens.front;
  if (tok.type == Token.Type.IDENT) {
    result = new Value(tok, tok.lineNum);
    tokens.popFront;
  }
  return tuple(result, tokens);
}

Tuple!(T, TokenRange) parseBinary(T, U, string[] ops, alias nextFunc)(TokenRange tokens) {
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


Tuple!(T, TokenRange) parseUnary(T, string[] ops, alias nextFunc)(TokenRange tokens) {
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

Tuple!(Expression4, TokenRange) parseExpression4(TokenRange tokens) {
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

Tuple!(Expression8, TokenRange) parseExpression8(TokenRange tokens) {
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

Tuple!(int, TokenRange) parseLiteral(TokenRange tokens) {
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

  int getVar(string name) {
    if (auto local = name in locals) return *local;

    foreach (frame; globalStack) {
      if (auto global = name in frame) return *global;
    }

    //TODO: when making these Variant objects, 
    return int.min;
  }

  void setLocalVar(string name, int val) {
    locals[name] = val;
  }
}

abstract class TreeNode {
  public int lineNum;
  public abstract int interpret(Context context) @safe; 
}

class Program : TreeNode {
  StmtSeq stmts;

  this(StmtSeq s, int ln) {
    stmts = s;
    lineNum = ln;
  }

  override int interpret(Context context) {
    return stmts.interpret(context);
  }
}

class StmtSeq : TreeNode {
  Statement[] stmts;

  this(Statement[] s, int ln) {
    stmts = s;
    lineNum = ln;
  }

  override int interpret(Context context) {
    int result = 0;
    foreach (stmt; stmts) {
      int interp = stmt.interpret(context);
      if (interp) result = interp;
    }
    return result;
  }
}

abstract class Statement : TreeNode {
  enum Type {
    ASSIGN, CALL, IF, WHILE, FOR, RETURN, FUNC, DEFINE
  }

  @property Type type() @safe;
}

class AssignStatement : Statement {
  Value lhs;
  Expression rhs;  

  this(Value val, Expression exp, int ln) {
    lhs = val;
    rhs = exp;
    lineNum = ln;
  }

  override int interpret(Context context) {
    context.setLocalVar(lhs.name, rhs.interpret(context));
    return 0;
  }

  @property override Type type() { return Statement.Type.ASSIGN; }
}

class CallStatement : Statement {
  
  @property override Type type() { return Statement.Type.CALL; }
}

class IfStatement : Statement {
  Expression cond;
  StmtSeq ifBody;

  Tuple!(Expression, StmtSeq)[] elseIfs;

  StmtSeq elseBody;

  this(Expression c, StmtSeq ib, Tuple!(Expression, StmtSeq)[] efs, StmtSeq eb, int ln) {
    cond = c;
    ifBody = ib;
    elseIfs = efs;
    elseBody = eb;
    lineNum = ln;
  }
  
  override int interpret(Context context) {
    if (cond.interpret(context)) {
      return ifBody.interpret(context);
    }

    foreach (ef; elseIfs) {
      if (ef[0].interpret(context)) {
        return ef[1].interpret(context);
      }
    }

    if (elseBody) {
      return elseBody.interpret(context);
    }

    return 0;
  }

  @property override Type type() { return Statement.Type.IF; }
}

class WhileStatement : Statement {
  
  @property override Type type() { return Statement.Type.WHILE; }
}

class ForStatement : Statement {
  
  @property override Type type() { return Statement.Type.FOR; }
}

class ReturnStatement : Statement {
  
  @property override Type type() { return Statement.Type.RETURN; }
}

class FuncStatement : Statement {

  @property override Type type() { return Statement.Type.FUNC; }
}

class DefineStatement: Statement {

  @property override Type type() { return Statement.Type.DEFINE; }
}

class Value : TreeNode {
  public string name;

  this(string s, int ln) { 
    name = s; 
    lineNum = ln;
  }

  override int interpret(Context context) {
    int result = context.getVar(name);

    if (result == int.min) runtimeError("No variable of name " ~ name ~ " has been assigned", lineNum);

    return result;
  }
}

class Expression : TreeNode {
  Expression2 sub;
  Tuple!(string, Expression2)[] extra;

  this(Expression2 s, Tuple!(string, Expression2)[] e, int ln) {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  override int interpret(Context context) {
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

  this(Expression3 s, Tuple!(string, Expression3)[] e, int ln) {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  override int interpret(Context context) {
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

  this(string o, Expression4 s, int ln) {
    op = o;
    sub = s;
    lineNum = ln;
  }

  override int interpret(Context context) {
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
  
  this(Expression5 s, Tuple!(string, Expression5) e, int ln) {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  override int interpret(Context context) {
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
  
  this(Expression6 s, Tuple!(string, Expression6)[] e, int ln) {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  override int interpret(Context context) {
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

  this(Expression7 s, Tuple!(string, Expression7)[] e, int ln) {
    sub = s;
    extra = e;
    lineNum = ln;
  }
  
  override int interpret(Context context) {
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
  
  this(string o, Expression8 s, int ln) {
    op = o;
    sub = s;
    lineNum = ln;
  }

  override int interpret(Context context) {
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

  this(int l, Value v, Expression s, int ln) {
    literal = l;
    val = v;
    sub = s;
    lineNum = ln;
  }

  override int interpret(Context context) {
    if (sub) return sub.interpret(context);
    else if (val) return val.interpret(context);
    return literal;
  }
}
