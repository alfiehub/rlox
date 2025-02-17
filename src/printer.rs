use crate::{
    ast::{Expression, Statement},
    visitor::Visitor,
};

struct Printer {
    depth: usize,
}

impl Printer {
    #[allow(dead_code)]
    fn new() -> Self {
        Self { depth: 0 }
    }

    #[allow(dead_code)]
    fn print(&mut self, decl: Vec<Statement>) -> String {
        decl.into_iter()
            .map(|d| self.visit_statement(d))
            .collect::<String>()
            .replace(';', ";\n")
            .replace('{', "{\n")
            .replace('}', "}\n")
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
            Expression::Literal(lit) => match lit.0.token_type {
                crate::token::TokenType::String(s) => format!("\"{s}\""),
                crate::token::TokenType::Number(n) => format!("{n}"),
                _ => lit.to_string(),
            },
            Expression::Assign(ident, expr) => {
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
            Expression::Variable(ident) => ident.to_string(),
            Expression::Get(expr, ident) => format!("{}.{ident}", self.visit_expression(*expr)),
            Expression::Set(expr, ident, value) => format!(
                "{}.{ident} = {}",
                self.visit_expression(*expr),
                self.visit_expression(*value)
            ),
            Expression::Super(ident, _) => ident.to_string(),
            Expression::This(ident) => ident.to_string(),
        }
    }

    fn visit_statement(&mut self, stmt: Statement) -> String {
        match stmt {
            Statement::Expression(expr) => format!("{};", self.visit_expression(expr)),
            Statement::Print(expr) => format!("print {};", self.visit_expression(expr)),
            Statement::If(condition, then_branch, else_branch) => {
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
            Statement::Block(decls) => {
                self.depth += 1;
                let output = format!(
                    "{{{}{}",
                    decls
                        .into_iter()
                        .map(|d| padded_format!(self.depth, "{}", self.visit_statement(d)))
                        .collect::<String>(),
                    padded_format!(self.depth - 1, "}}")
                );
                self.depth -= 1;
                output
            }
            Statement::While(expr, stmt) => {
                format!(
                    "while ({}) {}",
                    self.visit_expression(expr),
                    self.visit_statement(*stmt)
                )
            }
            Statement::Function(ident, arg_idents, body) => {
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
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    format!("return {};", self.visit_expression(expr))
                } else {
                    "return;".to_string()
                }
            }
            Statement::Variable(ident, expr) => {
                if let Some(expr) = expr {
                    format!("var {ident} = {};", self.visit_expression(expr))
                } else {
                    format!("var {ident};")
                }
            }
            Statement::Class(ident, superclass, methods) => {
                self.depth += 1;
                let superclass = superclass
                    .map(|expr| format!(" < {}", self.visit_expression(expr)))
                    .unwrap_or_default();
                let output = format!(
                    "class {ident}{superclass} {{{}{}",
                    methods
                        .into_iter()
                        .map(|d| padded_format!(
                            self.depth,
                            "{}",
                            &self.visit_statement(d)[4..] // hacky way to remove "fun "
                        ))
                        .collect::<String>(),
                    padded_format!(self.depth - 1, "}}")
                );
                self.depth -= 1;
                output
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
    fn test_statement_function() {
        let input = "\
fun a() {
  var i = 1;
  i = 2;
  return i;
}";
        let program = parse!(input, declaration);
        let mut ast_printer = Printer::new();
        assert_eq!(
            input.replace('\n', ""),
            ast_printer.visit_statement(program).replace('\n', "")
        );
    }

    macro_rules! test_file {
        ($fn:ident, $file:expr) => {
            #[test]
            fn $fn() {
                let input = std::fs::read_to_string(format!("scripts/{}.lox", $file)).unwrap();
                let program = parse!(&input);
                let mut ast_printer = Printer::new();
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
    test_file!(test_class, "class");
}
