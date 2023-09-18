use crate::{
    ast::Declaration,
    visitor::{DeclarationVisitor, ExpressionVisitor, StatementVisitor},
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

impl DeclarationVisitor<String> for Printer {
    fn variable(
        &mut self,
        ident: crate::ast::Identifier,
        expr: Option<crate::ast::Expression>,
    ) -> String {
        if let Some(expr) = expr {
            format!("var {ident} = {};", self.visit_expression(expr))
        } else {
            format!("var {ident};")
        }
    }

    fn statement(&mut self, stmt: crate::ast::Statement) -> String {
        self.visit_statement(stmt)
    }
}

impl ExpressionVisitor<String> for Printer {
    fn unary(&mut self, op: crate::ast::UnaryOperator, expr: crate::ast::Expression) -> String {
        format!("{op} {}", self.visit_expression(expr))
    }

    fn binary(
        &mut self,
        left_expr: crate::ast::Expression,
        op: crate::ast::Operator,
        right_expr: crate::ast::Expression,
    ) -> String {
        format!(
            "{} {op} {}",
            self.visit_expression(left_expr),
            self.visit_expression(right_expr)
        )
    }

    fn logical(
        &mut self,
        left_expr: crate::ast::Expression,
        op: crate::ast::Operator,
        right_expr: crate::ast::Expression,
    ) -> String {
        format!(
            "{} {op} {}",
            self.visit_expression(left_expr),
            self.visit_expression(right_expr)
        )
    }

    fn grouping(&mut self, expr: crate::ast::Expression) -> String {
        format!("({})", self.visit_expression(expr))
    }

    fn literal(&mut self, lit: crate::ast::Literal) -> String {
        let o = match lit.0.token_type {
            crate::token::TokenType::String(s) => format!("\"{s}\""),
            crate::token::TokenType::Number(n) => format!("{n}"),
            _ => lit.to_string(),
        };
        o
    }

    fn assignment(
        &mut self,
        ident: crate::ast::Identifier,
        expr: crate::ast::Expression,
    ) -> String {
        format!("{ident} = {}", self.visit_expression(expr))
    }

    fn call(&mut self, f: crate::ast::Expression, args: Vec<crate::ast::Expression>) -> String {
        format!(
            "{}({})",
            self.visit_expression(f),
            args.into_iter()
                .map(|arg| self.visit_expression(arg))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl StatementVisitor<String> for Printer {
    fn expression(&mut self, expr: crate::ast::Expression) -> String {
        format!("{};", self.visit_expression(expr))
    }

    fn print(&mut self, expr: crate::ast::Expression) -> String {
        format!("print {};", self.visit_expression(expr))
    }

    fn if_statement(
        &mut self,
        condition: crate::ast::Expression,
        then_branch: crate::ast::Statement,
        else_branch: Option<crate::ast::Statement>,
    ) -> String {
        let then_branch = self.visit_statement(then_branch);
        if let Some(else_branch) = else_branch {
            let else_branch = self.visit_statement(else_branch);
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

    fn block(&mut self, decls: Vec<crate::ast::Declaration>) -> String {
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

    fn while_statement(
        &mut self,
        expr: crate::ast::Expression,
        stmt: crate::ast::Statement,
    ) -> String {
        format!(
            "while ({}) {}",
            self.visit_expression(expr),
            self.visit_statement(stmt)
        )
    }

    fn function(
        &mut self,
        ident: crate::ast::Identifier,
        arg_idents: Vec<crate::ast::Identifier>,
        stmt: crate::ast::Statement,
    ) -> String {
        let block = self.visit_statement(stmt);
        format!(
            "fun {ident}({}) {}",
            arg_idents
                .into_iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            block
        )
    }

    fn return_statement(&mut self, expr: Option<crate::ast::Expression>) -> String {
        if let Some(expr) = expr {
            format!("return {};", self.visit_expression(expr))
        } else {
            format!("return;")
        }
    }
}

// impl Visitor for AstPrinter {}

#[cfg(test)]
mod tests {
    use crate::visitor::DeclarationVisitor;

    use super::Printer;
    use crate::parse;

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
