import std.stdio, std.variant;

alias Null = typeof(null);
alias RTVal = Algebraic!(bool, int, float, string, char, Null, This[]);

void main() {
  RTVal x = 3, y = 7;

  writeln(x.type.toString().ptr);
  writeln(y.type.toString().ptr);
}