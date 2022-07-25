import lexer, std.algorithm, std.typecons, std.string, std.range, std.conv, std.variant, std.sumtype;

//@safe:

int main(string[] args) {
  import std.stdio, std.file;

  if (args.length != 2) {
    writeln("Usage: parser <program file>");
    return 1;
  }

  auto programText = readText(args[1]);

  auto program = parse(programText);
  Context context = new Context;
  //program.interpret(context);

  writeln("Globals:");
  context.globals.each!((k, v) => writeln(k, ": ", v));
  writeln("\nLocals:");
  context.locals.each!((k, v) => writeln(k, ": ", v));

  return 0;
}

shared static this() {
  TOKEN_TO_PRIMITIVE = [
    "void": Primitive.void_,
    "u8" : Primitive.u8, "i8" : Primitive.i8,
    "u16" : Primitive.u16, "i16" : Primitive.i16,
    "u32" : Primitive.u32, "i32" : Primitive.i32,
    "u64" : Primitive.u64, "i64" : Primitive.i64,
    "f64" : Primitive.f64, "f32" : Primitive.f32,
    "bool" : Primitive.bool_,
    "char" : Primitive.char_,
    "string" : Primitive.string_,
  ];
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

  foreach (s; stmts) {
    if (auto rs = cast(ReturnStatement)s) {
      parseError("Return statements aren't allowed in the outer scope", s.lineNum);
    }
  }

  return tuple(new StmtSeq(stmts, lineNum), tokens);  
}

bool[string] innerStmtFollowSet = null;
bool[Statement.Type] innerStmtDisallowedSet = null;

Tuple!(StmtSeq, TokenRange) parseInnerStmtSeq(TokenRange tokens, bool allowReturn = false) {
  //dangit D, let me use immutable AAs
  if (!innerStmtFollowSet.length) {
    innerStmtFollowSet     = ["}" : true];
    //innerStmtDisallowedSet = [];
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
    case "while":
      result = parseWhileStatement(tokens);
      break;
    case "for":
      result = parseForStatement(tokens);
      break;
    case "return":
      result = parseReturnStatement(tokens);
      break;
    default:
      if (tokens.front.type == Token.Type.IDENT) {
        result = parseDeclareAssignCallStmt(tokens);
      }
      else {
        result = tuple(null, tokens);
      }
      break;
  }

  if (!result[0]) parseError("Invalid statement", tokens.front.lineNum);
  return tuple(result[0], result[1]);
}

Tuple!(Statement, TokenRange) parseDeclareStatement(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;

  if (tokens.front.type != Token.Type.IDENT) parseError("Expected identifier to begin declaration", tokens.front.lineNum);
  string name = tokens.front.str;
  tokens.popFront;

  if (tokens.front != ":") parseError("Expected ':' after declaration name", tokens.front.lineNum);
  tokens.popFront;

  TypeName* typeName;

  //if either is true, assume inferred
  if (!(tokens.front == ":" || tokens.front == "=")) {
    auto typeNameParse = parseTypeName(tokens);
    typeName = typeNameParse[0];
    tokens = typeNameParse[1];

    //variable without value
    if (tokens.front == ";") {
      tokens.popFront;
      return tuple(cast(Statement) new DeclareStatement(name, typeName, false, TopExpression.init, lineNum), tokens);
    }
  }

  bool _constant;

  if (tokens.front == ":") {
    _constant = true;
  }
  else if (tokens.front == "=") {
    _constant = false;
  }
  else if (typeName == null) {
    parseError("Expected ':', '=', or ';' in declaration", tokens.front.lineNum);
  }
  else {
    parseError("Expected ':' or '=' in declaration", tokens.front.lineNum);
  }

  tokens.popFront;

  auto topExpressionParse = parseTopExpression(tokens);
  tokens = topExpressionParse[1];

  bool shouldEndWithSemicolon = topExpressionParse[0].match!(
    (ProcExpression pe) => !pe.procBody,
    _                   => true
  );

  if (shouldEndWithSemicolon) {
    if (tokens.front != ";") {
      parseError("Expected ';' to end declaration", tokens.front.lineNum);
    }
    tokens.popFront;
  }

  return tuple(cast(Statement) new DeclareStatement(name, typeName, _constant, topExpressionParse[0], lineNum), tokens);
}

Tuple!(Statement, TokenRange) parseIfStatement(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;

  if (tokens.front != "if") parseError("Expected 'if' to begin if statement", lineNum);
  tokens.popFront;

  //if (tokens.front != "(") parseError("Expected '(' before if condition", tokens.front.lineNum);
  //tokens.popFront;

  auto cond = parseExpression(tokens);
  if (!cond[0]) parseError("Expected expression after 'if'", tokens.front.lineNum);
  tokens = cond[1];

  //if (tokens.front != ")") parseError("Expected ')' after if condition", tokens.front.lineNum);
  //tokens.popFront;

  if (tokens.front != "{") parseError("Expected '{' to continue if statement", tokens.front.lineNum);
  tokens.popFront;

  auto mainBody = parseInnerStmtSeq(tokens);
  tokens = mainBody[1];

  if (tokens.front != "}") parseError("Expected '}' to end if statement", tokens.front.lineNum);
  tokens.popFront;

  Tuple!(Expression, StmtSeq)[] elseIfs;
  while (tokens.length > 2 && tokens[0] == "else" && tokens[1] == "if") {
    tokens.popFront;
    tokens.popFront;

    //if (tokens.front != "(") parseError("Expected '(' before else if condition", tokens.front.lineNum);
    //tokens.popFront;

    auto elseIfCond = parseExpression(tokens);
    if (!elseIfCond[0]) parseError("Expected expression for else if condition", tokens.front.lineNum);
    tokens = elseIfCond[1];

    //if (tokens.front != ")") parseError("Expected ')' after else if condition", tokens.front.lineNum);
    //tokens.popFront;

    if (tokens.front != "{") parseError("Expected '{' to before else if body", tokens.front.lineNum);
    tokens.popFront;

    auto elseIfBody = parseInnerStmtSeq(tokens);
    tokens = elseIfBody[1];

    if (tokens.front != "}") parseError("Expected '}' to end else if statement", tokens.front.lineNum);
    tokens.popFront;

    elseIfs ~= tuple(elseIfCond[0], elseIfBody[0]);
  }

  StmtSeq elseBody;
  if (tokens.front == "else") {
    tokens.popFront;

    if (tokens.front != "{") parseError("Expected '{' before else body", tokens.front.lineNum);
    tokens.popFront;

    auto theElse = parseInnerStmtSeq(tokens);
    if (!theElse[0]) parseError("Expected statements after 'else'", tokens.front.lineNum);
    tokens = theElse[1];

    if (tokens.front != "}") parseError("Expected '}' to end else statement", tokens.front.lineNum);
    tokens.popFront;

    elseBody = theElse[0];
  }

  return tuple(
    cast(Statement) (new IfStatement(cond[0], mainBody[0], elseIfs, elseBody, lineNum)),
    tokens
  );
}

Tuple!(Statement, TokenRange) parseWhileStatement(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;
  if (tokens.front != "while") parseError("Expected 'while' to begin while statement", lineNum);
  tokens.popFront;

  //if (tokens.front != "(") parseError("Expected '(' before while condition", tokens.front.lineNum);
  //tokens.popFront;

  auto cond = parseExpression(tokens);
  if (!cond[0]) parseError("Expected expression for while condition", tokens.front.lineNum);
  tokens = cond[1];

  //if (tokens.front != ")") parseError("Expected ')' after while condition", tokens.front.lineNum);
  //tokens.popFront;

  if (tokens.front != "{)") parseError("Expected '{' before while body", tokens.front.lineNum);
  tokens.popFront;

  auto loopBody = parseInnerStmtSeq(tokens);
  if (!loopBody[0]) parseError("Expected statements for loop body", tokens.front.lineNum);
  tokens = loopBody[1];

  if (tokens.front != "}") parseError("Expected '}' to end while statement", tokens.front.lineNum);
  tokens.popFront;

  return tuple(cast(Statement)(new WhileStatement(cond[0], loopBody[0], lineNum)), tokens);
}

Tuple!(Statement, TokenRange) parseForStatement(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;
  if (tokens.front != "for") parseError("Expected 'for' to begin for statement", lineNum);
  tokens.popFront;

  if (tokens.front.type != Token.Type.IDENT) parseError("Expected identifier after 'for'", tokens.front.lineNum); 
  string loopVar = tokens.front.str;
  tokens.popFront;

  if (tokens.front != "in") parseError("Expected 'in' to continue for statement", tokens.front.lineNum);
  tokens.popFront;

  //TODO: implement arrays
  auto loopLow = parseExpression(tokens);
  if (!loopLow[0]) parseError("Expected expression for lower loop bound", tokens.front.lineNum);
  tokens = loopLow[1];

  if (tokens.front != "to") parseError("Expected 'to' to continue for statement", tokens.front.lineNum);
  tokens.popFront;

  auto loopHigh = parseExpression(tokens);
  if (!loopHigh[0]) parseError("Expected expression for higher loop bound", tokens.front.lineNum);
  tokens = loopHigh[1];

  if (tokens.front != "{") parseError("Expected '{' to continue for statement", tokens.front.lineNum);
  tokens.popFront;

  auto loopBody = parseInnerStmtSeq(tokens);
  if (!loopBody[0]) parseError("Expected statements for the for loop body", tokens.front.lineNum);
  tokens = loopBody[1];

  if (tokens.front != "}") parseError("Expected '}' to continue for statement", tokens.front.lineNum);
  tokens.popFront;

  return tuple(cast(Statement)(new ForStatement(loopVar, loopLow[0], loopHigh[0], loopBody[0], lineNum)), tokens);
}

Tuple!(TypeName*, TokenRange) parseTypeName(TokenRange tokens) {
  TypeName* result;
  TypeName** current = &result;

  while (tokens.front == "*" || tokens.front == "[") {
    string token = tokens.front;
    tokens.popFront;

    if (token == "*") {
      Pointer!TypeName ptr;
      *current = new TypeName(ptr);
      current = &ptr.base;
    }
    else if (token == "[") {
      bool notSlice = false;

      if (tokens.front == "..") {
        DynamicArray!TypeName dynamic;
        tokens.popFront;
        *current = new TypeName(dynamic);
        current = &dynamic.base;
        notSlice = true;
      }
      else if (tokens.front != "]") {
        FixedArray!TypeName fixed;
        auto expressionParse = parseExpression(tokens);
        fixed.length = expressionParse[0];
        tokens = expressionParse[1];
        *current = new TypeName(fixed);
        current = &fixed.base;
        notSlice = true;
      }

      if (tokens.front != "]") {
        parseError("Expected ']' in array type name, not" ~ tokens.front, tokens.front.lineNum);
      }

      tokens.popFront;

      if (!notSlice) {
        Slice!TypeName slice;
        *current = new TypeName(slice);
        current = &slice.base;
      }
    }
  }

  if (tokens.front.type == Token.Type.KEYWORD) {
    if (auto primitive = tokens.front in TOKEN_TO_PRIMITIVE) {
      *current = new TypeName(*primitive);
    }
    else {
      parseError("Expected a type name", tokens.front.lineNum);
    }
  }
  else if (tokens.front.type == Token.Type.IDENT) {
    //@TODO: Make this a full value
    *current = new TypeName(TypeNamePlaceholder(tokens.front));
  }
  else {
    parseError("Expected a type name", tokens.front.lineNum);
  }

  tokens.popFront;

  return tuple(result, tokens);
}

Tuple!(ProcExpression, TokenRange) parseProcExpression(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;
  if (tokens.front != "proc") parseError("Expected 'proc' to begin procedure expression", tokens.front.lineNum);
  tokens.popFront;
  
  if (tokens.front != "(") parseError("Expected '(' after procedure name", tokens.front.lineNum);
  tokens.popFront;

  ProcArg[] procArgs;
  while (tokens.front != ")") {
    ProcArg newArg;

    if (tokens.front.type != Token.Type.IDENT) parseError("Expected name for procedure argument", tokens.front.lineNum);
    newArg.name = tokens.front;
    tokens.popFront;

    if (tokens.front != ":") parseError("Expected ':' after procedure argument name", tokens.front.lineNum);
    tokens.popFront;

    auto argTypeParse = parseTypeName(tokens);
    newArg.type = argTypeParse[0];
    tokens = argTypeParse[1];

    if (tokens.front == "=") {
      tokens.popFront;

      auto exp = parseExpression(tokens);
      if (!exp[0]) parseError("Expected expression for default arg value", tokens.front.lineNum);
      newArg.defaultVal = exp[0];
      tokens            = exp[1];
    }

    if (tokens.front == ",") {
      tokens.popFront;
    }
    else if (tokens.front != ")") {
      parseError("Unexpected token '" ~ tokens.front ~ "' in procedure args list", tokens.front.lineNum);
    }

    procArgs ~= newArg;
  }

  tokens.popFront;

  TypeName* returnType;

  if (tokens.front == "->") {
    tokens.popFront;

    auto returnTypeParse = parseTypeName(tokens);
    if (!returnTypeParse[0]) parseError("Expected return type for procedure", tokens.front.lineNum);
    returnType = returnTypeParse[0];
    tokens = returnTypeParse[1];
  }

  StmtSeq procBodySeq = null;

  if (tokens.front == "{") {
    tokens.popFront;

    auto procBody = parseInnerStmtSeq(tokens, true);
    if (!procBody[0]) parseError("Expected statements for procedure body", tokens.front.lineNum);
    procBodySeq = procBody[0];
    tokens      = procBody[1];

    if (tokens.front != "}") parseError("Expected '}' to end procedure definition", tokens.front.lineNum);
    tokens.popFront;
  }

  return tuple(new ProcExpression(procArgs, procBodySeq, returnType, lineNum), tokens);
}

Tuple!(Statement, TokenRange) parseReturnStatement(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;
  if (tokens.front != "return") parseError("Expected 'return' to begin return statement", lineNum);
  tokens.popFront;

  auto exp = parseExpression(tokens);
  if (!exp[0]) parseError("Expected expression after 'return'", lineNum);
  tokens = exp[1];

  if (tokens.front != ";") parseError("Expected ';' to end return statement", lineNum);
  tokens.popFront;

  return tuple(cast(Statement)(new ReturnStatement(exp[0], lineNum)), tokens);
}

Tuple!(Statement, TokenRange) parseDeclareAssignCallStmt(TokenRange tokens) {
  if (tokens[0].type == Token.Type.IDENT && tokens[1] == ":") {
    return parseDeclareStatement(tokens);
  }

  auto value = parseValue(tokens);
  if (!value[0]) return tuple(cast(Statement)null, tokens);
  tokens = value[1];

  if (tokens.front == "=") {
    return parseAssignmentExpression(tokens, value[0]);
  }
  else if (tokens.front == ";") {
    if (value[0].endsWithFuncCall()) {
      tokens.popFront;
      return tuple(
        cast(Statement)(new CallStatement(value[0], value[0].lineNum)),
        tokens
      );
    }
    else {
      parseError("Statement starting with a value must end with a procedure call", value[0].lineNum);
    }
  }
  else {
    parseError("Unexpected token '" ~ tokens.front ~ "' following value", tokens.front.lineNum);
  }

  return tuple(cast(Statement)null, tokens);
}

Tuple!(Statement, TokenRange) parseAssignmentExpression(TokenRange tokens, Value val) {
  if (tokens.front != "=") return tuple(cast(Statement)null, tokens);
  //if (tokens.front != "=") parseError("Expected '=' for assignment statement");
  tokens.popFront;

  auto exp = parseExpression(tokens);
  if (!exp[0]) parseError("Expected expression for assignment statement", tokens.front.lineNum);
  tokens = exp[1];

  if (tokens.front != ";") parseError("Expected ';' to end assignment statement", tokens.front.lineNum);
  tokens.popFront;

  return tuple(
    cast(Statement) (new AssignExpression(val, exp[0], val.lineNum)),
    tokens
  );
}

Tuple!(Value, TokenRange) parseValue(TokenRange tokens) {
  int lineNum = tokens.front.lineNum;
  Value result;

  if (tokens.front.type != Token.Type.IDENT) return tuple(result, tokens);
  string varName = tokens.front;
  tokens.popFront;

  Accessor[] accessors;

  accessorLoop:
  while (true) {
    int accLineNum = tokens.front.lineNum;

    switch (tokens.front) {
      case ".":
        tokens.popFront;
        
        if (tokens.front.type != Token.Type.IDENT) parseError("Expected identifier after '.'", tokens.front.lineNum);

        accessors ~= new DotAccessor(tokens.front, accLineNum);

        tokens.popFront;
      break;

      case "[":
        tokens.popFront;

        auto exp = parseExpression(tokens);
        if (!exp[0]) parseError("Expected expression after '['", tokens.front.lineNum);
        tokens = exp[1];

        accessors ~= new SubscriptAccessor(exp[0], accLineNum);

        if (tokens.front != "]") parseError("Expected ']' to end subscript", tokens.front.lineNum);
        tokens.popFront;
      break;

      case "(":
        tokens.popFront;

        Expression[] args;

        bool firstOne = true;
        while (tokens.front != ")") {
          if (!firstOne) {
            if (tokens.front != ",") parseError("Expected ',' to separate procedure args", tokens.front.lineNum);
            tokens.popFront;
          }

          firstOne = false;

          auto exp = parseExpression(tokens);
          if (!exp[0]) parseError("Expected expression or ')' after '('", tokens.front.lineNum);
          tokens = exp[1];
          args ~= exp[0];
        }

        tokens.popFront;

        accessors ~= new CallAccessor(args, accLineNum);
      break;

      default:
        break accessorLoop;
      break;
    }
  }

  result = new Value(varName, accessors, lineNum);

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

Tuple!(TopExpression, TokenRange) parseTopExpression(TokenRange tokens) {
  switch (tokens.front.str) {
    case "proc":
      auto procExpressionParse = parseProcExpression(tokens);
      return tuple(TopExpression(procExpressionParse[0]), procExpressionParse[1]);
      break;

    default:
      auto expressionParse = parseExpression(tokens);
      return tuple(TopExpression(expressionParse[0]), expressionParse[1]);
  }
}

alias parseExpression  = parseBinary!(Expression,  Expression2, ["or"], parseExpression2);
alias parseExpression2 = parseBinary!(Expression2, Expression3, ["and"], parseExpression3);
alias parseExpression3 = parseUnary!(Expression3, ["not"], parseExpression4);
alias parseExpression5 = parseBinary!(Expression5, Expression6, ["+", "-", "|"], parseExpression6);
alias parseExpression6 = parseBinary!(Expression6, Expression7, ["*", "/", "div", "mod", "&", "^"], parseExpression7);
alias parseExpression7 = parseUnary!(Expression7, ["-", "~"], parseExpression8);

Tuple!(Expression4, TokenRange) parseExpression4(TokenRange tokens) {
  static immutable ops = ["==", "!=", ">", "<", ">=", "<="];

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
  Literal literal; 
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
  if (potentialLiteral[0])  {
    literal = potentialLiteral[0];
    tokens  = potentialLiteral[1];
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

Tuple!(Literal, TokenRange) parseLiteral(TokenRange tokens) {
  RTVal rtVal;
  Literal result;
  int lineNum = tokens.front.lineNum;
  
  switch (tokens.front.type) {
    case Token.Type.INT:
      rtVal = tokens.front.to!int;
      tokens.popFront;
    break;
   
    case Token.Type.FLOAT:
      rtVal = tokens.front.to!float;
      tokens.popFront;
    break;
   
    case Token.Type.STRING:
      rtVal = tokens.front[1..$-1]; //TODO: escape sequences
      tokens.popFront;
    break;
   
    case Token.Type.CHAR:
      rtVal = cast(char) tokens.front[1..$-1][0]; //TODO: escape sequences
      tokens.popFront;
    break;
    
    case Token.Type.BOOL:
      rtVal = tokens.front.to!bool;
      tokens.popFront;
    break;

    case Token.Type.NULL:
      rtVal = null;
      tokens.popFront;
    break;

    default:
      auto arr = parseArray(tokens);
      if (!arr[0]) return tuple(cast(Literal)null, tokens);
      result = arr[0];
      tokens = arr[1];
    break;
  }

  if (!result) {
    result = new SingleLiteral(rtVal, lineNum);
  }

  //TODO: Change this to null when Variants are implemented
  return tuple(result, tokens);
}

Tuple!(ArrayLiteral, TokenRange) parseArray(TokenRange tokens) {
  Expression[] exps;
  ArrayLiteral result;

  int lineNum = tokens.front.lineNum;

  if (tokens.front != "[") return tuple(result, tokens);
  tokens.popFront;

  while (tokens.front != "]") {
    auto exp = parseExpression(tokens);
    if (!exp[0]) parseError("Expected expression in array literal", tokens.front.lineNum);
    tokens = exp[1];
    exps ~= exp[0];

    if (tokens.front == ",") tokens.popFront;
    else if (tokens.front != "]") {
      parseError("Unexpected token '" ~ tokens.front ~ "' in array literal", tokens.front.lineNum); 
    }
  }

  tokens.popFront;

  result = new ArrayLiteral(exps, lineNum);
  return tuple(result, tokens);
}

////////////////////////
// Runtime type stuff //
////////////////////////

interface procedure {
  RTVal call(Context context, RTVal[] args, int lineNum);
}

class RTFunction : procedure {
  StmtSeq procBody;
  ProcArg[] procArgs;
  TypeName* returnType;

  this(StmtSeq b, ProcArg[] a, TypeName* r) {
    procBody = b;
    procArgs = a;
    returnType = r;
  }

  override RTVal call(Context context, RTVal[] args, int lineNum) {
    if (procArgs.length != args.length) {
      runtimeError(format("Can't call a procedure taking %d args with %d args", procArgs.length, args.length), lineNum);
    }

    Context funcContext = context.newLocalContext;

    foreach (i, arg; args) {
      //if (procArgs[i].type != "" && procArgs[i].type != arg.type.toString) {
      //  runtimeError(
      //    format("Argument %s must be of type %s, not %s", procArgs[i].name, procArgs[i].type, arg.type.toString),
      //    lineNum
      //  );
      //}

      funcContext.setLocalVar(procArgs[i].name, arg);
    }
    
    auto returnVal = procBody.interpret(funcContext);

    //if (returnType != "" && returnType != returnVal.type.toString) {
    //  runtimeError(
    //    format("procedure is meant to return a value of tyoe %s, not %s", returnType, returnVal.type.toString),
    //    lineNum
    //  );
    //}

    return returnVal;
  }
}

/*class BuiltInFunction(alias func) : procedure {
  override RTVal call(Context context, RTVal[] args, int lineNum) {
    static if (is(ReturnType!func == void)) {

    }
    else {

    }
  }
}
*/
alias Null = typeof(null);
alias RTVal = Algebraic!(bool, int, float, string, char, Null, RTFunction, std.variant.This[]);

bool toBool(RTVal val) {
  return val.visit!(
    (bool b)       => b,
    (int i)        => i != 0,
    (float f)      => f != 0,
    (string s)     => s.length != 0,
    (char c)       => c != '\0',
    (Null n)       => false,
    (RTFunction f) => f !is null,
    (RTVal[] a)    => a.length != 0,
  );
}


/////////////////
// AST Classes //
/////////////////

class Context {
  RTVal[string] globals;
  RTVal[string] locals;

  RTVal getVar(string name) {
    if (auto local = name in locals) return *local;

    if (auto global = name in globals) return *global;

    //TODO: when making these Variant objects, 
    return RTVal();
  }

  bool hasLocalVar(string name) { return cast(bool)(name in locals); }

  void setLocalVar(string name, RTVal val) {
    locals[name] = val;
  }

  void setGlobalVar(string name, RTVal val) {
    globals[name] = val;
  }

  void deleteLocalVar(string name) {
    locals.remove(name);
  }

  Context newLocalContext() {
    Context result = new Context;
    result.globals = globals;
    return result;
  }
}

abstract class TreeNode {
  public int lineNum;
  public abstract RTVal interpret(Context context); 
}

class Program : TreeNode {
  StmtSeq stmts;

  this(StmtSeq s, int ln) {
    stmts = s;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    return stmts.interpret(context);
  }
}

class StmtSeq : TreeNode {
  Statement[] stmts;

  this(Statement[] s, int ln) {
    stmts = s;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    RTVal interp;
    foreach (i, stmt; stmts) {
      interp = stmt.interpret(context);
    }
    return interp;
  }
}

abstract class Statement : TreeNode {
  enum Type {
    ASSIGN, CALL, IF, WHILE, FOR, RETURN, FUNC, DECLARE
  }

  @property Type type() @safe;
}

class AssignExpression : Statement {
  Value lhs;
  Expression rhs;  

  this(Value val, Expression exp, int ln) {
    lhs = val;
    rhs = exp;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    if (lhs.accessors.length) {
      RTVal rhs           = rhs.interpret(context);
      RTVal thingToAssign = context.getVar(lhs.name);
      RTVal* runner       = &thingToAssign;
      RTVal callResult;
      bool assignable = false;

      foreach (acc; lhs.accessors) {
        switch (acc.type) {
          case Accessor.Type.DOT:
            runtimeError("Dot operator not implemented.", lineNum);
          break;

          case Accessor.Type.SUBSCRIPT:
            if (runner.type != typeid(RTVal[])) runtimeError("Can't subscript a non-array", acc.lineNum);

            auto index = (cast(SubscriptAccessor)acc).inside.interpret(context);
            if (index.type != typeid(int)) runtimeError("Subscript index must be an int", acc.lineNum);

            runner = &(runner.get!(RTVal[])[index.get!int]);
            assignable = true;
          break;

          case Accessor.Type.CALL:
            if (runner.type != typeid(RTFunction)) runtimeError("Can't call a non-procedure", acc.lineNum);
            
            callResult = acc.interpret(*runner, context);
            runner     = &callResult;
            assignable = callResult.type != typeid(RTVal[]);
          break;

          default: break;
        }
      }

      if (!assignable) runtimeError("Left-hand side is not assignable", lineNum);

      *runner = rhs;
    }
    else {
      context.setLocalVar(lhs.name, rhs.interpret(context));
    }

    return RTVal();
  }

  @property override Type type() { return Statement.Type.ASSIGN; }
}

class CallStatement : Statement {
  Value value;

  this(Value v, int ln) {
    value = v;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    return value.interpret(context);
  }

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
  
  override RTVal interpret(Context context) {
    if (cond.interpret(context).toBool) {
      ifBody.interpret(context);
      return RTVal();
    }

    foreach (ef; elseIfs) {
      if (ef[0].interpret(context).toBool) {
        ef[1].interpret(context);
        return RTVal();
      }
    }

    if (elseBody) {
      elseBody.interpret(context);
      return RTVal();
    }

    return RTVal();
  }

  @property override Type type() { return Statement.Type.IF; }
}

class WhileStatement : Statement {
  Expression cond;
  StmtSeq loopBody;

  this(Expression c, StmtSeq l, int ln) {
    cond = c;
    loopBody = l;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    RTVal result = 0;
    while (cond.interpret(context).toBool) {
      result = 1;
      loopBody.interpret(context);
    }
    return result;
  }
  
  @property override Type type() { return Statement.Type.WHILE; }
}

class ForStatement : Statement {
  string loopVar;
  Expression low, high;
  StmtSeq loopBody;

  this(string lv, Expression l, Expression h, StmtSeq lb, int ln) {
    loopVar = lv;
    low = l;
    high = h;
    loopBody = lb;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    if (context.hasLocalVar(loopVar)) {
      runtimeError("A local variable named '" ~ loopVar ~ "' has already been defined", lineNum);
    }

    auto lowVal  = low.interpret(context);
    if (lowVal.type != typeid(int)) {
      runtimeError("For loop bounds must be of type int", lineNum);
    }

    auto highVal = high.interpret(context);
    if (highVal.type != typeid(int)) {
      runtimeError("For loop bounds must be of type int", lineNum);
    }

    if (lowVal > highVal) {
      runtimeError("For loop low bound must be smaller than the high bound", lineNum);
    }

    foreach (x; lowVal..highVal+1) {

      context.setLocalVar(loopVar, x);
      loopBody.interpret(context);
    }

    context.deleteLocalVar(loopVar);

    return RTVal();
  }

  @property override Type type() { return Statement.Type.FOR; }
}

class ReturnStatement : Statement {
  Expression exp;

  this(Expression e, int ln) {
    exp = e;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    return exp.interpret(context);
  }

  @property override Type type() { return Statement.Type.RETURN; }
}

alias ProcArg = Tuple!(string, "name", TypeName*, "type", Expression, "defaultVal");
class ProcExpression : Statement {
  ProcArg[] args;
  StmtSeq procBody;
  TypeName* returnType;

  this(ProcArg[] a, StmtSeq fb, TypeName* rt, int ln) {
    args = a;
    procBody = fb;
    returnType = rt;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    //context.setGlobalVar(name, RTVal(new RTFunction(procBody, args, returnType)));

    return RTVal();
  }

  @property override Type type() { return Statement.Type.FUNC; }
}

class DeclareStatement : Statement {
  string name;
  TypeName* typeName;
  bool constant;
  TopExpression expression;
  int lineNum;

  this(string n, TypeName* t, bool c, TopExpression e, int ln) {
    name = n;
    typeName = t;
    this.constant = c;
    expression = e;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    return RTVal();
  }

  @property override Type type() { return Statement.Type.DECLARE; }
}

//class DefType {
//  bool[string] memMap;
//  string[] memArr;

//  DefTypeVal makeNew() {

//  }
//}

//struct DefTypeVal {
//  RTVal*[string] valMap;
//  RTVal[] vals;

//  RTVal dot(string member) {

//  }
//}

class Value : TreeNode {
  public string name;
  Accessor[] accessors;

  this(string s, Accessor[] a, int ln) { 
    name = s; 
    accessors = a;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    RTVal result = context.getVar(name);

    if (!result.hasValue) runtimeError("No variable of name " ~ name ~ " has been assigned", lineNum);

    foreach (acc; accessors) {
      result = acc.interpret(result, context);
    }

    return result;
  }

  bool endsWithFuncCall() {
    return accessors.length && accessors[$-1].type == Accessor.Type.CALL;
  }
}

abstract class Accessor {
  enum Type {
    DOT,
    SUBSCRIPT,
    CALL
  }

  int lineNum;

  abstract RTVal interpret(RTVal calling, Context context);
  abstract @property Type type();
}

class DotAccessor : Accessor {
  string memberName;

  this(string mn, int ln) {
    memberName = mn;
    lineNum = ln;
  }

  override RTVal interpret(RTVal calling, Context context) {
    runtimeError("Dot operator not yet implemented.", lineNum);
    return RTVal();
  }

  override @property Type type() { return Accessor.Type.DOT; }
}

class SubscriptAccessor : Accessor {
  Expression inside;

  this(Expression i, int ln) {
    inside = i;
    lineNum = ln;
  }

  override RTVal interpret(RTVal calling, Context context) {
    if (calling.type != typeid(RTVal[])) runtimeError("Can't subspcript a non-array!", lineNum);
    
    auto index = inside.interpret(context);
    if (index.type != typeid(int)) runtimeError("Subscript must be an int", inside.lineNum);

    return calling.get!(RTVal[])[index.get!int];
  }

  override @property Type type() { return Accessor.Type.SUBSCRIPT; }
}

class CallAccessor : Accessor {
  Expression[] args;

  this(Expression[] as, int ln) {
    args = as;
    lineNum = ln;
  }

  override RTVal interpret(RTVal calling, Context context) {
    if (calling.type != typeid(RTFunction)) runtimeError("Can't call a non-procedure", lineNum);

    RTVal[] argVals = new RTVal[](args.length);

    foreach (i, arg; args) {
      argVals[i] = arg.interpret(context);
    }

    return calling.get!RTFunction.call(context, argVals, lineNum);
  }

  override @property Type type() { return Accessor.Type.CALL; }
}

alias TopExpression = SumType!(ProcExpression, Expression);

class Expression : TreeNode {
  Expression2 sub;
  Tuple!(string, Expression2)[] extra;

  this(Expression2 s, Tuple!(string, Expression2)[] e, int ln) {
    sub = s;
    extra = e;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    RTVal result = sub.interpret(context);

    foreach (x; extra) {
      if (result.toBool) {
        //short circuit
        result = true;
        break;
      }

      if (x[0] == "or") {
        if (x[1].interpret(context).toBool) {
          result = true;
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

  override RTVal interpret(Context context) {
    RTVal result = sub.interpret(context);

    foreach (x; extra) {
      if (!result.toBool) break; //short circuit

      if (x[0] == "and") {
        if (x[1].interpret(context).toBool) result = true;
        else result = false;
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

  override RTVal interpret(Context context) {
    if (op == "not") {
      if (sub.interpret(context).toBool) return RTVal(false);
      return RTVal(true);
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

  override RTVal interpret(Context context) {
    RTVal result = sub.interpret(context);

    if (!extra[1]) return result;
    RTVal rhs = extra[1].interpret(context);

    //make sure that our defined truthiness gets used when comparing a bool, not D's
    if (result.type != rhs.type) {
      if (result.type == typeid(bool)) {
        rhs = rhs.toBool;
      }
      else if (rhs.type == typeid(bool)) {
        result = result.toBool;
      }
    }

    switch (extra[0]) {
      case "==":
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

  override RTVal interpret(Context context) {
    RTVal result = sub.interpret(context);

    foreach (x; extra) {
      switch (x[0]) {
        case "+":
          result += x[1].interpret(context);        
        break;

        case "-":
          result -= x[1].interpret(context);        
        break;

        case "|":
          auto rhs = x[1].interpret(context);

          if (!result.convertsTo!int || !rhs.convertsTo!int) {
            runtimeError("With the | operator, both sides must convert to ints", lineNum);
          }

          result = cast(int) (result.coerce!int | rhs.coerce!int);        
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
  
  override RTVal interpret(Context context) {
    RTVal result = sub.interpret(context);

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
          auto rhs = x[1].interpret(context);

          if (!result.convertsTo!int || !rhs.convertsTo!int) {
            runtimeError("With the & operator, both sides must convert to ints", lineNum);
          }

          result = cast(int) (result.coerce!int & rhs.coerce!int);    
        break;

        case "^":
          auto rhs = x[1].interpret(context);

          if (!result.convertsTo!int || !rhs.convertsTo!int) {
            runtimeError("With the ^ operator, both sides must convert to ints", lineNum);
          }

          result = cast(int) (result.coerce!int ^ rhs.coerce!int);     
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

  override RTVal interpret(Context context) {
    if (op == "-") {
      auto val = sub.interpret(context);

      if (val.type == typeid(int)) {
        return RTVal(-val.get!int);
      }
      else if (val.type == typeid(float)) {
        return RTVal(-val.get!float);
      }
      else {
        runtimeError("With the - operator, the argument must be numeric", lineNum);
      }
    }
    else if (op == "~") {
      auto val = sub.interpret(context);

      if (!val.convertsTo!int) {
        runtimeError("With the ~ operator, the argument must convert to an int", lineNum);
      }

      return RTVal(cast(int) (~val.coerce!int));
    }

    return sub.interpret(context);
  }
}

class Expression8 : TreeNode {
  Literal literal;
  Value val;
  Expression sub;

  this(Literal l, Value v, Expression s, int ln) {
    literal = l;
    val = v;
    sub = s;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    if (sub) return sub.interpret(context);
    else if (val) return val.interpret(context);
    return literal.interpret(context);
  }
}

class Literal : TreeNode { }

class SingleLiteral : Literal {
  RTVal rtVal;

  this(RTVal v, int ln) {
    rtVal = v;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    return rtVal;
  }
}

class ArrayLiteral : Literal {
  Expression[] exps;

  this(Expression[] es, int ln) {
    exps = es;
    lineNum = ln;
  }

  override RTVal interpret(Context context) {
    RTVal[] rtVals = new RTVal[](exps.length);

    foreach (i, e; exps) {
      rtVals[i] = e.interpret(context);
    }

    return RTVal(rtVals);
  }
}

enum Primitive {
  void_,
  u8, i8,
  u16, i16,
  u32, i32,
  u64, i64,
  f32, f64,
  bool_,
  char_,
  string_,
}

Primitive[string] TOKEN_TO_PRIMITIVE;
struct Pointer(T) {

  T* base;
}

struct FixedArray(T) {
  T* base;
  Expression length;
}

struct Slice(T) {
  T* base;
}

struct DynamicArray(T) {
  T* base;
}

struct Class(T) {
  string name;
  T[] members;
  string[] memberNames;
}

struct Union(T) {
  string name;
  T[] members;
  string[] memberNames;
}

struct TypeNamePlaceholder {
  string name;
}

alias TypeName = SumType!(
  Primitive, TypeNamePlaceholder, Class!(std.sumtype.This), Union!(std.sumtype.This), Pointer!(std.sumtype.This),
  FixedArray!(std.sumtype.This), Slice!(std.sumtype.This), DynamicArray!(std.sumtype.This)
);