use crate::log;
use advancing_vec::AdvVec;
pub use lexing::*;
pub use tree::*;

mod advancing_vec;
pub mod lexing;
mod tree;

macro_rules! push_repeats {
    ($parse: expr, from $data: ident to $target: ident, seperated by $token: expr ) => {
        while let Some(item) = $parse {
            $target.push(item);
            if !$data.advance_if_eq($token) {
                break;
            }
        }
    };

    ($parse: expr, from $data: ident to $target: ident, must be seperated by $token: expr ) => {
        if let Some(item) = $parse {
            $target.push(item);
        }

        while $data.advance_if_eq($token) {
            $target.push($parse?);
        }
    };
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! log {
    ($($e: expr),*) => {
        {
            use std::sync::atomic::Ordering as _ORD;
            use $crate::desmos::parsing::advancing_vec::GUARDS_ACTIVE as _ACTIVE;

            for _ in 0..(_ACTIVE.load(_ORD::Relaxed)) {
                print!("| ");
            }
            println!($($e),*);
        }
    };
}

#[cfg(not(feature = "log"))]
#[macro_export]
macro_rules! log {
    ($($e: expr),*) => {};
}

macro_rules! return_if_cond {
    (
        with me $self: ident:
        if let Some($id: ident) = $expr: expr;
        and $cond: expr;
        return $ret: expr;
    ) => {
        let _guard = $self.data.guard_denied();
        if let Some($id) = $expr {
            if $cond {
                _guard.accepted();
                return $ret;
            }
        }
        drop(_guard);
    };

    (
        with me $self: ident:
        if let Some($id: ident) = $expr: expr;
        and $cond: expr;
        return some $ret: expr;
    ) => {
        let _guard = $self.data.guard_denied();
        if let Some($id) = $expr {
            if $cond {
                return Some($ret($id));
            }
        }
        drop(_guard);
    };
}

pub struct Parser {
    data: AdvVec<Token>,
}

impl Parser {
    const fn is_skip_token(token: &Token) -> bool {
        matches!(
            token,
            Token::Punct(Punctuation::Comma)
                | Token::Punct(Punctuation::RightParen)
                | Token::Punct(Punctuation::RightSquare)
                | Token::Punct(Punctuation::RightCurly)
                | Token::Punct(Punctuation::RightLatexSquare)
                | Token::Punct(Punctuation::RightLatexCurly)
                | Token::Punct(Punctuation::Exp)
                | Token::Punct(Punctuation::Equals)
                | Token::Punct(Punctuation::Colon)
                | Token::Punct(Punctuation::For)
                | Token::Punct(Punctuation::Prime)
                | Token::Punct(Punctuation::Ellipsis)
        )
    }

    pub fn parse(tokens: Vec<Token>) -> Option<Statement> {
        let mut parser = Self::new(tokens);
        // println!("\nENDING STATE: {}\n", parser.data);
        parser.parse_statement()
    }

    pub fn parse_expr(tokens: Vec<Token>) -> Option<Expression> {
        let mut parser = Self::new(tokens);
        // println!("\nENDING STATE: {}\n", parser.data);
        parser.parse_expression()
    }

    pub fn parse_action_expr(tokens: Vec<Token>) -> Option<ActExpr> {
        let mut parser = Self::new(tokens);
        parser.parse_act_expr()
    }

    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            data: AdvVec::new(tokens),
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        log!("parsing statement");
        log!("status: {:?}", self.data);

        let guard = self.data.guard_accepted();

        return_if_cond!(
            with me self:
            if let Some(def) = self.parse_function_def();
            and self.data.is_done();
            return some Statement::Function;
        );
        return_if_cond!(
            with me self:
            if let Some(def) = self.parse_act_func_def();
            and self.data.is_done();
            return some Statement::ActFunction;
        );
        return_if_cond!(
            with me self:
            if let Some(def) = self.parse_variable_def();
            and self.data.is_done();
            return some Statement::Variable;
        );
        return_if_cond!(
            with me self:
            if let Some(def) = self.parse_act_var_def();
            and self.data.is_done();
            return some Statement::ActVar;
        );
        return_if_cond!(
            with me self:
            if let Some(def) = self.parse_expression();
            and self.data.is_done();
            return some Statement::Expression;
        );
        return_if_cond!(
            with me self:
            if let Some(def) = self.parse_act_expr();
            and self.data.is_done();
            return some Statement::ActExpr;
        );

        guard.denied();
        None
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        if let Some(Token::Identifier(s)) = unsafe { self.data.peek() } {
            unsafe {
                self.data.advance();
            }
            log!("SUCCESS: Identifier parsed");
            Some(Ident(s.clone()))
        } else {
            log!("NO IDENTIFIER");
            None
        }
    }

    fn parse_number(&mut self) -> Option<Number> {
        log!("parsing number");
        log!("status: {:?}", self.data);
        let guard = self.data.guard_denied();
        let next = guard.advance()?;
        if let Token::Number(n) = next {
            guard.accepted();
            Some(Number(*n))
        } else {
            None
        }
    }

    fn parse_function_def(&mut self) -> Option<FunctionDef> {
        log!("parsing function_def");
        log!("status: {:?}", self.data);
        let guard = self.data.guard_denied();
        let name = self.parse_ident()?;
        guard.eq_else_none(&Token::Punct(Punctuation::LeftParen))?;

        let mut params = Vec::new();

        push_repeats!(
            self.parse_ident(),
            from guard to params,
            seperated by &Token::Punct(Punctuation::Comma)
        );

        guard.eq_else_none(&Token::Punct(Punctuation::RightParen))?;
        guard.eq_else_none(&Token::Punct(Punctuation::Equals))?;

        let expr = self.parse_expression()?;

        guard.accepted();
        Some(FunctionDef {
            func: name,
            params,
            expr,
        })
    }

    fn parse_variable_def(&mut self) -> Option<VariableDef> {
        log!("parsing variable_def");
        log!("status: {:?}", self.data);
        let guard = self.data.guard_denied();
        let ident = self.parse_ident()?;
        guard.eq_else_none(&Token::Punct(Punctuation::Equals))?;
        let expr = self.parse_expression()?;
        guard.accepted();
        Some(VariableDef { ident, expr })
    }

    fn parse_conditional(&mut self) -> Option<Conditional> {
        log!("parsing conditional");
        log!("status: {:?}", self.data);
        let data = self.data.guard_denied();

        let expr = self.parse_expression()?;
        let next = data.peek()?;
        match next {
            Token::Punct(Punctuation::Equals) => {
                let mut exprs = vec![expr];

                while data.advance_if_eq(&Token::Punct(Punctuation::Equals)) {
                    exprs.push(self.parse_expression()?);
                }

                data.accepted();
                Some(Conditional::Equality(Equality { exprs }))
            }
            Token::Punct(Punctuation::LessThan)
            | Token::Punct(Punctuation::MoreThan)
            | Token::Punct(Punctuation::LessOrEqual)
            | Token::Punct(Punctuation::MoreOrEqual) => {
                let mut exprs = vec![expr];
                let mut kinds = vec![];

                while let Some(Token::Punct(punct)) = data.peek() {
                    if let Some(kind) = InequalityType::from_punct(punct) {
                        data.advance();
                        kinds.push(kind);
                        exprs.push(self.parse_expression()?);
                    } else {
                        break;
                    }
                }

                data.accepted();
                Some(Conditional::Inequality(Inequality { exprs, kinds }))
            }
            _ => None,
        }
    }

    fn parse_latex_grouping(&mut self) -> Option<Expression> {
        log!("parsing latex_grouping");
        log!("status: {:?}", self.data);
        log!("Starting latex!");
        let data = self.data.guard_denied();
        data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly))?;
        let expr = self.parse_expression()?;
        log!("expr: {expr:?}");
        log!("state: {:?}", self.data);
        data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly))?;
        data.accepted();
        log!("Latex SUCCESS!");
        Some(expr)
    }

    fn parse_list_contents(&mut self) -> Option<ListContents> {
        log!("parsing list_contents");
        log!("status: {:?}", self.data);

        if unsafe { self.data.peek() } == Some(&Token::Punct(Punctuation::RightSquare)) {
            return Some(ListContents::Literal(ListLiteral { parts: vec![] }));
        }

        let data = self.data.guard_denied();
        let first_expr = self.parse_expression()?;
        let next = data.advance()?;
        if let Token::Punct(Punctuation::For) = next {
            let mut statements = Vec::new();

            push_repeats!(
                self.parse_variable_def(),
                from data to statements,
                seperated by &Token::Punct(Punctuation::Comma)
            );

            data.accepted();
            Some(ListContents::Comprehension(ListComprehension {
                expr: first_expr,
                statements,
            }))
        } else if let Token::Punct(Punctuation::RightSquare) = next {
            data.un_advance();
            data.accepted();
            Some(ListContents::Literal(ListLiteral {
                parts: vec![first_expr],
            }))
        } else if let Token::Punct(Punctuation::Comma) = next {
            let mut exprs = vec![first_expr];

            push_repeats!(
                self.parse_expression(),
                from data to exprs,
                seperated by &Token::Punct(Punctuation::Comma)
            );

            let restored = self.data.guard_accepted();
            let next = data.advance()?;
            if let Token::Punct(Punctuation::RightSquare) = next {
                restored.denied();
                drop(restored);
                data.accepted();
                Some(ListContents::Literal(ListLiteral { parts: exprs }))
            } else if let Token::Punct(Punctuation::Ellipsis) = next {
                data.advance_if_eq(&Token::Punct(Punctuation::Comma));

                let mut after_exprs = Vec::new();
                push_repeats!(
                    self.parse_expression(),
                    from data to after_exprs,
                    seperated by &Token::Punct(Punctuation::Comma)
                );

                data.accepted();
                Some(ListContents::Range(ListRange {
                    before: exprs,
                    after: after_exprs,
                }))
            } else {
                None
            }
        } else if let Token::Punct(Punctuation::Ellipsis) = next {
            data.advance_if_eq(&Token::Punct(Punctuation::Comma));

            let mut after_exprs = Vec::new();
            push_repeats!(
                self.parse_expression(),
                from data to after_exprs,
                seperated by &Token::Punct(Punctuation::Comma)
            );

            data.accepted();
            Some(ListContents::Range(ListRange {
                before: vec![first_expr],
                after: after_exprs,
            }))
        } else {
            None
        }
    }

    fn parse_everything_else(&mut self) -> Option<EverythingElse> {
        log!("parsing everything_else");
        log!("status: {:?}", self.data);
        let data = self.data.guard_denied();
        let next = data.advance()?;
        log!("first: {next:?}");
        match next {
            Token::Punct(Punctuation::LeftParen) => {
                let first_expr = self.parse_expression()?;
                match data.advance()? {
                    Token::Punct(Punctuation::RightParen) => {
                        data.accepted();
                        Some(EverythingElse::Grouping(Grouping { expr: first_expr }))
                    }
                    Token::Punct(Punctuation::Comma) => {
                        let second_expr = self.parse_expression()?;

                        data.eq_else_none(&Token::Punct(Punctuation::RightParen))?;

                        data.accepted();
                        Some(EverythingElse::Point(Point {
                            x: first_expr,
                            y: second_expr,
                        }))
                    }
                    _ => None,
                }
            }
            Token::Punct(Punctuation::LeftSquare) => {
                let list = self.parse_list_contents()?;
                data.eq_else_none(&Token::Punct(Punctuation::RightSquare))?;
                data.accepted();
                Some(EverythingElse::List(list))
            }
            Token::Punct(Punctuation::LeftCurly) => {
                let cond = self.parse_conditional()?;

                let branches = data
                    .advance_if_eq(&Token::Punct(Punctuation::Colon))
                    .then(|| {
                        let yes = self.parse_expression()?;

                        let no = data
                            .advance_if_eq(&Token::Punct(Punctuation::Comma))
                            .then(|| self.parse_expression())
                            .flatten();

                        Some(IfElseBranches { yes, no })
                    })
                    .flatten();

                data.eq_else_none(&Token::Punct(Punctuation::RightCurly))?;
                data.accepted();
                Some(EverythingElse::IfElse(IfElse { cond, branches }))
            }
            Token::Punct(Punctuation::LeftAbs) => {
                let expr = self.parse_expression()?;
                data.eq_else_none(&Token::Punct(Punctuation::RightAbs))?;
                data.accepted();
                Some(EverythingElse::Abs(AbsoluteValue { expr }))
            }
            Token::Punct(Punctuation::Frac) => {
                let success_guard = self.data.guard_denied();
                let success = Some(())
                    .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly)))
                    .and_then(|_| data.eq_else_none(&Token::Identifier("d".to_string())))
                    .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly)))
                    .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly)))
                    .and_then(|_| data.eq_else_none(&Token::Identifier("dx".to_string())))
                    .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly)))
                    .is_some();
                success.then(|| {
                    success_guard.accepted();
                });
                drop(success_guard);

                if success {
                    let expr = self.parse_multiply()?;

                    data.accepted();
                    Some(EverythingElse::Differentiate(Differentiate {
                        expr: Box::new(expr),
                    }))
                } else {
                    let top = self.parse_latex_grouping()?;
                    let bottom = self.parse_latex_grouping()?;
                    data.accepted();
                    Some(EverythingElse::Fraction(Fraction { top, bottom }))
                }
            }
            // Token::Punct(Punctuation::Frac) => {
            //     let success = Some(())
            //         .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly)))
            //         .and_then(|_| data.eq_else_none(&Token::Identifier("d".to_string())))
            //         .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly)))
            //         .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly)))
            //         .and_then(|_| data.eq_else_none(&Token::Identifier("dx".to_string())))
            //         .and_then(|_| data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly)))
            //         .is_some();
            //
            //     if success {
            //         let expr = self.parse_multiply()?;
            //
            //         data.accepted();
            //         Some(PrefixOrBelow::Differentiate(Differentiate { expr }))
            //     } else {
            //         data.denied();
            //         drop(data);
            //
            //         let mult = self.parse_multiply()?;
            //         Some(PrefixOrBelow::Below(mult))
            //     }
            // }
            Token::Punct(Punctuation::Sum) | Token::Punct(Punctuation::Prod) => {
                let kind = match next {
                    Token::Punct(Punctuation::Sum) => SumOrProduct::Sum,
                    Token::Punct(Punctuation::Prod) => SumOrProduct::Product,
                    _ => unreachable!(),
                };

                data.eq_else_none(&Token::Punct(Punctuation::Subscript))?;
                data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly))?;
                let VariableDef {
                    ident: counter,
                    expr: from,
                } = self.parse_variable_def()?;
                data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly))?;

                data.eq_else_none(&Token::Punct(Punctuation::Exp))?;
                let to = self.parse_latex_grouping()?;

                let expr = self.parse_multiply()?;

                data.accepted();
                // None
                Some(EverythingElse::SumProd(SumProd {
                    kind,
                    expr: Box::new(expr),
                    counter,
                    from,
                    to,
                }))
            }
            Token::Punct(Punctuation::Sqrt) => {
                let nth = if data.advance_if_eq(&Token::Punct(Punctuation::LeftLatexSquare)) {
                    let root = self.parse_number()?;
                    data.eq_else_none(&Token::Punct(Punctuation::RightLatexSquare))?;
                    root.0
                } else {
                    2.0
                };

                let expr = self.parse_latex_grouping()?;

                data.accepted();
                Some(EverythingElse::Root(Root { nth, expr }))
            }
            Token::Identifier(func) => {
                let func = func.clone();
                let mut prime_count = 0;

                while data.advance_if_eq(&Token::Punct(Punctuation::Prime)) {
                    prime_count += 1;
                }

                let power = if data.advance_if_eq(&Token::Punct(Punctuation::Exp)) {
                    data.eq_else_none(&Token::Punct(Punctuation::LeftLatexCurly))?;
                    let expr = self.parse_expression()?;
                    data.eq_else_none(&Token::Punct(Punctuation::RightLatexCurly))?;

                    Some(expr)
                } else {
                    None
                };

                if data.advance_if_eq(&Token::Punct(Punctuation::LeftParen)) || prime_count > 0 {
                    let mut exprs = Vec::new();

                    push_repeats!(
                        self.parse_expression(),
                        from data to exprs,
                        seperated by &Token::Punct(Punctuation::Comma)
                    );

                    data.eq_else_none(&Token::Punct(Punctuation::RightParen))?;

                    data.accepted();
                    Some(EverythingElse::Call(FunctionCall {
                        func: Ident(func),
                        params: exprs,
                        prime_count,
                        power,
                    }))
                } else {
                    let expr = EverythingElse::Ident(Ident(func));
                    data.accepted();
                    match power {
                        None => Some(expr),
                        Some(power) => Some(EverythingElse::Grouping(Grouping {
                            expr: Expression {
                                expr: Box::new(Everything::Below(MultiplyOrBelow::Below(
                                    PostfixOrBelow::Power(Power {
                                        base: Box::new(PostfixOrBelow::Below(expr)),
                                        power,
                                    }),
                                ))),
                            },
                        })),
                    }
                }
            }
            &Token::Number(num) => {
                data.accepted();
                Some(EverythingElse::Number(Number(num)))
            }
            _ => None,
        }
    }

    fn parse_postfix(&mut self) -> Option<PostfixOrBelow> {
        log!("parsing postfix");

        // skip immediately because multiplication will always parse this after
        let token = unsafe { self.data.peek() }?;
        if Parser::is_skip_token(token) {
            log!("SKIPPING TOKEN {token:?}");
            return None;
        }

        log!("status: {:?}", self.data);
        let mut current = PostfixOrBelow::Below(self.parse_everything_else()?);

        loop {
            let data = self.data.guard_denied();
            match data.advance() {
                Some(Token::Punct(Punctuation::Exp)) => {
                    log!("Exp!");
                    let power = self.parse_latex_grouping()?;
                    log!("Latex success!");
                    data.accepted();
                    current = PostfixOrBelow::Power(Power {
                        base: Box::new(current),
                        power,
                    });
                }
                Some(Token::Punct(Punctuation::LeftSquare)) => {
                    if let Some(index) = (|| {
                        let guard = self.data.guard_denied();
                        let index = self.parse_expression()?;
                        guard.eq_else_none(&Token::Punct(Punctuation::RightSquare))?;
                        guard.accepted();
                        Some(index)
                    })() {
                        data.accepted();
                        current = PostfixOrBelow::Indexing(ListIndexing {
                            list: Box::new(current),
                            index,
                        });
                    } else if let Some(list) = (|| {
                        let guard = self.data.guard_denied();
                        let index = self.parse_list_contents()?;
                        guard.eq_else_none(&Token::Punct(Punctuation::RightSquare))?;
                        guard.accepted();
                        Some(index)
                    })() {
                        data.accepted();
                        current = PostfixOrBelow::Indexing(ListIndexing {
                            list: Box::new(current),
                            index: Expression {
                                expr: Box::new(Everything::Below(MultiplyOrBelow::Below(
                                    PostfixOrBelow::Below(EverythingElse::List(list)),
                                ))),
                            },
                        });
                    } else if let Some(filter) = (|| {
                        let guard = self.data.guard_denied();
                        let index = self.parse_conditional()?;
                        guard.eq_else_none(&Token::Punct(Punctuation::RightSquare))?;
                        guard.accepted();
                        Some(index)
                    })() {
                        data.accepted();
                        current = PostfixOrBelow::Filtering(ListFiltering {
                            list: Box::new(current),
                            filter,
                        });
                    } else {
                        return None;
                    }
                }
                Some(Token::Punct(Punctuation::Dot)) => {
                    let Ident(ident) = self.parse_ident()?;

                    let element = match ident.as_str() {
                        "x" => Element::X,
                        "y" => Element::Y,
                        _ => return None,
                    };

                    data.accepted();

                    current = PostfixOrBelow::Element(ElementAccess {
                        expr: Box::new(current),
                        element,
                    });
                }
                _ => break Some(current),
            }
        }
    }

    fn parse_multiply(&mut self) -> Option<MultiplyOrBelow> {
        log!("parsing multiply");
        log!("status: {:?}", self.data);

        let first = self.parse_postfix()?;

        let data = self.data.guard_denied();

        let mut exprs = vec![first];

        loop {
            if data.advance_if_eq(&Token::Punct(Punctuation::Mult)) {
                let next = self.parse_postfix()?;
                exprs.push(next);
            } else if let Some(next) = self.parse_postfix() {
                exprs.push(next);
            } else {
                break;
            }
        }

        data.accepted();

        Some(if exprs.len() == 1 {
            let [first] = exprs.try_into().unwrap();
            MultiplyOrBelow::Below(first)
        } else {
            MultiplyOrBelow::Multiply(Multiply { exprs })
        })
    }

    fn parse_everything(&mut self) -> Option<Everything> {
        log!("parsing everything");
        log!("status: {:?}", self.data);
        let data = self.data.guard_denied();

        let first_type = data.advance_if_some(AddOrSub::from_token);

        let first = self.parse_multiply()?;
        if data.peek().and_then(AddOrSub::from_token).is_some() {
            let mut exprs = vec![first];
            let mut types = vec![first_type.unwrap_or(AddOrSub::Add)];

            while let Some(kind) = data.peek().and_then(AddOrSub::from_token) {
                data.advance();
                types.push(kind);
                exprs.push(self.parse_multiply()?);
            }

            data.accepted();
            Some(Everything::AddSub(AddSub { exprs, types }))
        } else if let Some(kind) = first_type {
            data.accepted();
            Some(Everything::AddSub(AddSub {
                exprs: vec![first],
                types: vec![kind],
            }))
        } else {
            data.accepted();
            Some(Everything::Below(first))
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        log!("parsing expression");
        log!("status: {:?}", self.data);
        self.parse_everything()
            .map(|v| Expression { expr: Box::new(v) })
    }

    fn parse_act_var_def(&mut self) -> Option<ActVarDef> {
        log!("parsing act_var_def");

        let data = self.data.guard_denied();
        let ident = self.parse_ident()?;
        data.eq_else_none(&Token::Punct(Punctuation::Equals))?;
        let expr = self.parse_act_expr()?;

        data.accepted();
        Some(ActVarDef { ident, expr })
    }

    fn parse_act_func_def(&mut self) -> Option<ActFuncDef> {
        log!("parsing act_func_def");

        let data = self.data.guard_denied();
        let func = self.parse_ident()?;
        data.eq_else_none(&Token::Punct(Punctuation::LeftParen))?;
        let mut params = Vec::new();
        push_repeats!(
            self.parse_ident(),
            from data to params,
            seperated by &Token::Punct(Punctuation::Comma)
        );
        data.eq_else_none(&Token::Punct(Punctuation::RightParen))?;
        data.eq_else_none(&Token::Punct(Punctuation::Equals))?;
        let expr = self.parse_act_expr()?;

        data.accepted();
        Some(ActFuncDef { func, params, expr })
    }

    fn parse_act_expr(&mut self) -> Option<ActExpr> {
        log!("parsing act_expr");

        let mut actions = Vec::new();

        let data = self.data.guard_denied();

        push_repeats!(
            self.parse_action(),
            from data to actions,
            seperated by &Token::Punct(Punctuation::Comma)
        );

        (!actions.is_empty()).then(|| {
            data.accepted();
            ActExpr { actions }
        })
    }

    fn parse_action(&mut self) -> Option<Action> {
        log!("parsing action");

        let data = self.data.guard_denied();
        match data.advance().cloned()? {
            Token::Identifier(ident) => {
                if data.advance_if_eq(&Token::Punct(Punctuation::SimArrow)) {
                    let expr = self.parse_expression()?;

                    data.accepted();
                    Some(Action::Raw(RawAction {
                        ident: Ident(ident),
                        expr,
                    }))
                } else if data.advance_if_eq(&Token::Punct(Punctuation::LeftParen)) {
                    let mut params = vec![];

                    push_repeats!(
                        self.parse_expression(),
                        from data to params,
                        seperated by &Token::Punct(Punctuation::Comma)
                    );
                    data.eq_else_none(&Token::Punct(Punctuation::RightParen))?;

                    data.accepted();
                    Some(Action::Call(ActFuncCall {
                        func: Ident(ident),
                        params,
                    }))
                } else {
                    data.accepted();
                    Some(Action::Ident(Ident(ident)))
                }
            }
            Token::Punct(Punctuation::LeftCurly) => {
                let cond = self.parse_conditional()?;
                data.eq_else_none(&Token::Punct(Punctuation::Colon))?;
                let yes = self.parse_act_expr()?;
                if data.advance_if_eq(&Token::Punct(Punctuation::Comma)) {
                    let no = self.parse_act_expr()?;
                    data.eq_else_none(&Token::Punct(Punctuation::RightCurly))?;

                    data.accepted();
                    Some(Action::IfElse(ActIfElse {
                        cond,
                        yes,
                        no: Some(no),
                    }))
                } else {
                    data.eq_else_none(&Token::Punct(Punctuation::RightCurly))?;

                    data.accepted();
                    Some(Action::IfElse(ActIfElse {
                        cond,
                        yes,
                        no: None,
                    }))
                }
            }
            Token::Punct(Punctuation::LeftParen) => {
                let expr = self.parse_act_expr()?;
                data.eq_else_none(&Token::Punct(Punctuation::RightParen))?;
                data.accepted();
                Some(Action::Grouping(ActGrouping { expr }))
            }
            _ => None,
        }
    }
}
