use crate::{
    ast::{Declaration, Expression},
    visitor::Visitor,
};

struct Printer {
    depth: usize,
}

impl Printer {
    fn new() -> Self {
        Self { depth: 0 }
    }

    fn print(&mut self, decl: Vec<Declaration>) -> String {
        decl.into_iter()
            .map(|d| self.visit_declaration(d))
            .collect::<String>()
            .replace(";", ";\n")
            .replace("{", "{\n")
            .replace("}", "}\n")
            // unfuck else
            .replace("}\n else", "} else")
    }
}

macro_rules! padded_format {
    ($depth:expr, $($arg:tt)*) => {{
        format!("{}{}", "  ".to_string().repeat($depth), format!($($arg)*))
    }}
}

impl Visitor<String> for Printer {
    fn visit_declaration(&mut self, decl: Declaration) -> String {
        match decl {
            Declaration::Variable(ident, expr) => {
                if let Some(expr) = expr {
                    format!("var {ident} = {};", self.visit_expression(expr))
                } else {
                    format!("var {ident};")
                }
            }
            Declaration::Statement(stmt) => self.visit_statement(stmt),
        }
    }

    fn visit_expression(&mut self, expr: Expression) -> String {
        match expr {
            Expression::Unary(op, expr) => {
                format!("{op} {}", self.visit_expression(*expr))
            }
            Expression::Binary(left_expr, op, right_expr) => format!(
                "{} {op} {}",
                self.visit_expression(*left_expr),
                self.visit_expression(*right_expr)
            ),
            Expression::Logical(left_expr, op, right_expr) => format!(
                "{} {op} {}",
                self.visit_expression(*left_expr),
                self.visit_expression(*right_expr)
            ),
            Expression::Grouping(expr) => format!("({})", self.visit_expression(*expr)),
            Expression::Literal(lit) => {
                let o = match lit.0.token_type {
                    crate::token::TokenType::String(s) => format!("\"{s}\""),
                    crate::token::TokenType::Number(n) => format!("{n}"),
                    _ => lit.to_string(),
                };
                o
            }
            Expression::Assignment(ident, expr) => {
                format!("{ident} = {}", self.visit_expression(*expr))
            }
            Expression::Call(f, args) => {
                format!(
                    "{}({})",
                    self.visit_expression(*f),
                    args.into_iter()
                        .map(|arg| self.visit_expression(arg))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }

    fn visit_statement(&mut self, stmt: crate::ast::Statement) -> String {
        match stmt {
            crate::ast::Statement::Expression(expr) => format!("{};", self.visit_expression(expr)),
            crate::ast::Statement::Print(expr) => format!("print {};", self.visit_expression(expr)),
            crate::ast::Statement::If(condition, then_branch, else_branch) => {
                let then_branch = self.visit_statement(*then_branch);
                if let Some(else_branch) = else_branch {
                    let else_branch = self.visit_statement(*else_branch);
                    format!(
                        "if ({}) {} else {}",
                        self.visit_expression(condition),
                        then_branch,
                        else_branch
                    )
                } else {
                    format!("if ({}) {}", self.visit_expression(condition), then_branch)
                }
            }
            crate::ast::Statement::Block(decls) => {
                self.depth += 1;
                let output = format!(
                    "{{{}{}",
                    decls
                        .into_iter()
                        .map(|d| padded_format!(self.depth, "{}", self.visit_declaration(d)))
                        .collect::<String>(),
                    padded_format!(self.depth - 1, "}}")
                );
                self.depth -= 1;
                output
            }
            crate::ast::Statement::While(expr, stmt) => {
                format!(
                    "while ({}) {}",
                    self.visit_expression(expr),
                    self.visit_statement(*stmt)
                )
            }
            crate::ast::Statement::Function(ident, arg_idents, body) => {
                let body = self.visit_statement(*body);
                format!(
                    "fun {ident}({}) {}",
                    arg_idents
                        .into_iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                )
            }
            crate::ast::Statement::Return(expr) => {
                if let Some(expr) = expr {
                    format!("return {};", self.visit_expression(expr))
                } else {
                    format!("return;")
                }
            }
        }
    }
}

// impl Visitor for AstPrinter {}

#[cfg(test)]
mod tests {
    use super::Printer;
    use crate::parse;
    use crate::visitor::Visitor;

    #[test]
    fn test_declaration_function() {
        let input = "\
fun a() {
  var i = 1;
  i = 2;
  return i;
}";
        let program = parse!(input, declaration);
        let mut ast_printer = Printer::new();
        assert_eq!(
            input.replace("\n", ""),
            ast_printer.visit_declaration(program).replace("\n", "")
        );
    }

    macro_rules! test_file {
        ($fn:ident, $file:expr) => {
            #[test]
            fn $fn() {
                let input = std::fs::read_to_string(format!("scripts/{}.lox", $file)).unwrap();
                let program = parse!(&input);
                let mut ast_printer = Printer::new();
                println!("{}", ast_printer.print(program.clone()));
                assert_eq!(
                    input.replace("\n", ""),
                    ast_printer.print(program).replace("\n", "")
                );
            }
        };
    }
    test_file!(test_if_else, "if_else");
    test_file!(test_nested_fun, "nested_fun");
    test_file!(test_redeclare, "redeclare");
    test_file!(test_block_scope, "block_scope");
    test_file!(test_functions, "functions");
}