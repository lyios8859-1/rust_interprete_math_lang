use std::collections::HashMap;

//----------------------------------Lexer start------------------------------------------
#[derive(Debug, PartialEq)]
enum TokenType {
  NumberLiteral, // 2, 3, 35
  Identifier,    // a, b, c, aComplicatedVariableName
  Equal,
  Plus,
  Minus,
  Star,
  Slash,
  LeftParen,
  RightParen,
  Newline,
}

#[derive(Debug)]
struct Token {
  token_type: TokenType,
  lexeme: String, // 123 -> lexeme: "123"
}

fn tokenize(source_code: &str) -> Vec<Token> {
  let mut position = 0;
  let mut result = Vec::new();

  while position < source_code.len() {
    let current_char = source_code.chars().nth(position).unwrap();

    match current_char {
      '=' => result.push(Token {
        token_type: TokenType::Equal,
        lexeme: "=".to_string(),
      }),
      '+' => result.push(Token {
        token_type: TokenType::Plus,
        lexeme: "+".to_string(),
      }),
      '-' => result.push(Token {
        token_type: TokenType::Minus,
        lexeme: "-".to_string(),
      }),
      '*' => result.push(Token {
        token_type: TokenType::Star,
        lexeme: "*".to_string(),
      }),
      '/' => result.push(Token {
        token_type: TokenType::Slash,
        lexeme: "/".to_string(),
      }),
      '(' => result.push(Token {
        token_type: TokenType::LeftParen,
        lexeme: "(".to_string(),
      }),
      ')' => result.push(Token {
        token_type: TokenType::RightParen,
        lexeme: ")".to_string(),
      }),
      '\n' => result.push(Token {
        token_type: TokenType::Newline,
        lexeme: '\n'.to_string(),
      }),
      x if x.is_digit(10) => {
        let mut number_lexeme = x.to_string();

        position += 1;

        while position < source_code.len() {
          let next_char = source_code.chars().nth(position).unwrap();

          if next_char == ' ' || next_char == ')' || next_char == '\n' {
            break;
          }
          if next_char.is_digit(10) {
            number_lexeme.push(next_char);
          } else {
            panic!("Invalid character: '{}'", next_char);
          }
          position += 1;
        }
        result.push(Token {
          token_type: TokenType::NumberLiteral,
          lexeme: number_lexeme,
        });
        continue;
      }
      ' ' => {}
      c => {
        let mut lexeme = c.to_string();

        position += 1;

        while position < source_code.len() {
          let next_char = source_code.chars().nth(position).unwrap();

          if !is_valid_identifier_char(next_char) {
            break;
          }
          lexeme.push(next_char);
          position += 1;
        }
        result.push(Token {
          token_type: TokenType::Identifier,
          lexeme,
        });
        continue;
      }
    }
    position += 1;
  }

  return result;
}

fn is_valid_identifier_char(ch: char) -> bool {
  return ch.is_alphanumeric() || ch == '_';
}
//----------------------------------Lexer end------------------------------------------

//----------------------------------Parser start----------------------------------------
/*
a = 2
b = 3
c = a * 2 + b - 6
print(c)


expression: assignment
assignment: (Variable "=")? expression | term
term: factor ( ("+" | "-") factor )*
factor: primary ( ("*" | "/") primary )*
primary: Variable | Number | FunCall | "(" expression ")"
FunCall: Variable "(" expression ")"
*/

struct AssignmentImpl {
  // a = 34
  target: VariableImpl,
  value: Box<Expr>,
}

struct BinaryOpImpl {
  // a + b
  lhs: Box<Expr>,
  operation: Token,
  rhs: Box<Expr>,
}

struct FunCallImpl {
  name: VariableImpl,
  arg: Box<Expr>,
}

struct NumberImpl {
  // 34
  value: i32,
  token: Token,
}

struct VariableImpl {
  // a
  name: Token,
}

enum Expr {
  Assignment(AssignmentImpl),
  BinaryOperation(BinaryOpImpl),
  FunCall(FunCallImpl),
  Number(NumberImpl),
  Variable(VariableImpl),
}

fn parse(mut tokens: Vec<Token>) -> Vec<Expr> {
  tokens.reverse();

  let mut result = Vec::new();

  while tokens.len() > 0 {
    let expr = parse_expr(&mut tokens);

    expect(TokenType::Newline, &mut tokens);

    result.push(expr);
  }

  return result;
}

fn parse_expr(tokens: &mut Vec<Token>) -> Expr {
  return parse_assignment(tokens);
}

fn parse_assignment(tokens: &mut Vec<Token>) -> Expr {
  // 向前看一下是不是等号（赋值）
  let next_token = &tokens[tokens.len() - 2];
  if tokens.len() > 1 && next_token.token_type == TokenType::Equal {
    let variable = parse_variable(tokens);
    expect(TokenType::Equal, tokens);
    let value = parse_expr(tokens);
    return Expr::Assignment(AssignmentImpl {
      target: variable,
      value: Box::new(value),
    });
  } else {
    return parse_term(tokens);
  }
}

fn parse_variable(tokens: &mut Vec<Token>) -> VariableImpl {
  let token = tokens.pop().unwrap();
  if token.token_type == TokenType::Identifier {
    return VariableImpl { name: token };
  } else {
    panic!("Expected Identifier, found {:?}", token.token_type);
  }
}

fn parse_term(tokens: &mut Vec<Token>) -> Expr {
  let mut result = parse_factor(tokens);

  while tokens.len() > 0 {
    let next_token = &tokens[tokens.len() - 1];
    match next_token.token_type {
      TokenType::Plus | TokenType::Minus => {
        let op_token = tokens.pop().unwrap();
        let rhs = parse_factor(tokens);

        result = Expr::BinaryOperation(BinaryOpImpl {
          lhs: Box::new(result),
          operation: op_token,
          rhs: Box::new(rhs),
        })
      }
      _ => break,
    }
  }

  return result;
}

fn parse_factor(tokens: &mut Vec<Token>) -> Expr {
  let mut result = parse_primary(tokens);

  while tokens.len() > 1 {
    let next_token = &tokens[tokens.len() - 1];
    match next_token.token_type {
      TokenType::Star | TokenType::Slash => {
        let op_token = tokens.pop().unwrap();
        let rhs = parse_primary(tokens);

        result = Expr::BinaryOperation(BinaryOpImpl {
          lhs: Box::new(result),
          operation: op_token,
          rhs: Box::new(rhs),
        });
      }
      _ => break,
    }
  }

  return result;
}

fn parse_primary(tokens: &mut Vec<Token>) -> Expr {
  let token = tokens.pop().unwrap();

  match token.token_type {
    TokenType::NumberLiteral => {
      return Expr::Number(NumberImpl {
        value: parse_number(&token.lexeme),
        token,
      })
    }
    TokenType::Identifier => {
      if tokens.len() > 0 {
        let next_token = &tokens[tokens.len() - 1];
        if next_token.token_type == TokenType::LeftParen {
          let func_name = VariableImpl { name: token };

          tokens.pop().unwrap();

          let arg = parse_expr(tokens);

          expect(TokenType::RightParen, tokens);

          return Expr::FunCall(FunCallImpl {
            name: func_name,
            arg: Box::new(arg),
          });
        }
      }
      return Expr::Variable(VariableImpl { name: token });
    }
    TokenType::LeftParen => {
      let expr = parse_expr(tokens);
      expect(TokenType::RightParen, tokens);
      return expr;
    }
    other => panic!("Unexpected token type: {:?}", other),
  }
}

fn parse_number(s: &str) -> i32 {
  return s.parse::<i32>().unwrap();
}

fn expect(expected: TokenType, tokens: &mut Vec<Token>) {
  match tokens.pop() {
    None => {}
    Some(token) => {
      if token.token_type != expected {
        panic!("Expected token {:?}, got {:?}", expected, token.token_type);
      }
    }
  }
}
//----------------------------------Parser end------------------------------------------

//----------------------------------Evaluate start---------------------------------------
type Env = HashMap<String, i32>;

fn interpret(exprs: &Vec<Expr>, env: &mut Env) {
  for expr in exprs {
    evaluate(expr, env);
  }
}

fn evaluate(expr: &Expr, env: &mut Env) -> i32 {
  match expr {
    Expr::Assignment(AssignmentImpl { target, value }) => {
      let value = evaluate(value, env);

      env.insert(target.name.lexeme.clone(), value);

      /*
      a = 2
      a = b = 2
      */
      return value;
    }
    Expr::BinaryOperation(BinaryOpImpl {
      lhs,
      operation,
      rhs,
    }) => {
      let lhs_value = evaluate(lhs, env);
      let rhs_value = evaluate(rhs, env);

      match &operation.token_type {
        TokenType::Plus => return lhs_value + rhs_value,
        TokenType::Minus => return lhs_value - rhs_value,
        TokenType::Star => return lhs_value * rhs_value,
        TokenType::Slash => return lhs_value / rhs_value,
        other => panic!("Invalid binary operation: {:?}", other),
      }
    }
    Expr::FunCall(FunCallImpl { name, arg }) => {
      if name.name.lexeme == "print" {
        let value = evaluate(arg, env);
        println!("{}", value);

        return value;
      } else {
        panic!("Undefined function {}", name.name.lexeme);
      }
    }
    Expr::Variable(VariableImpl { name }) => {
      if let Some(value) = env.get(&name.lexeme) {
        return *value;
      } else {
        panic!("Variable {} isn't defined", name.lexeme)
      }
    }
    Expr::Number(NumberImpl { value, .. }) => return *value,
  }
}
//----------------------------------Evaluate end-----------------------------------------

fn main() {
  let src = "a_123 = 2 * 3 -1
             print(a_123)";
  let tokens = tokenize(src);

  // for token in tokens {
  //   println!("{:?}", token);
  // }

  let exprs = parse(tokens);

  let mut env: Env = HashMap::new();
  interpret(&exprs, &mut env);
}
