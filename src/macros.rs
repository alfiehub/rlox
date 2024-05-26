#[macro_export]
macro_rules! parse {
    ($program:expr) => {{
        let tokens = $crate::scanner::Scanner::scan($program).unwrap();
        let mut parser = $crate::parser::Parser::new(tokens);
        parser.parse().unwrap()
    }};
    ($program:expr, $to_parse:ident) => {{
        let tokens = $crate::scanner::Scanner::scan($program).unwrap();
        let mut parser = $crate::parser::Parser::new(tokens);
        parser.$to_parse().unwrap()
    }};
}
