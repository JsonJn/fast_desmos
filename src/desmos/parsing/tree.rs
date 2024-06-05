use super::{Punctuation, Token};

#[derive(Debug, Clone)]
pub struct Ident(pub String);
#[derive(Debug, Clone)]
pub struct Number(pub f64);

#[derive(Debug, Clone)]
pub enum Statement {
    Function(FunctionDef),
    Variable(VariableDef),
    ActFunction(ActFuncDef),
    ActVar(ActVarDef),
    Expression(Expression),
    ActExpr(ActExpr),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub func: Ident,
    pub params: Vec<Ident>,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct VariableDef {
    pub ident: Ident,
    pub expr: Expression,
}

// ACTION PARSE TREE

#[derive(Debug, Clone)]
pub struct ActFuncDef {
    pub func: Ident,
    pub params: Vec<Ident>,
    pub expr: ActExpr,
}

#[derive(Debug, Clone)]
pub struct ActVarDef {
    pub ident: Ident,
    pub expr: ActExpr,
}

#[derive(Debug, Clone)]
pub struct ActGrouping {
    pub expr: ActExpr,
}

#[derive(Debug, Clone)]
pub struct ActExpr {
    pub actions: Vec<Action>,
}

#[derive(Debug, Clone)]
pub struct ActFuncCall {
    pub func: Ident,
    pub params: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Action {
    Raw(RawAction),
    IfElse(ActIfElse),
    Grouping(ActGrouping),
    Ident(Ident),
    Call(ActFuncCall),
}

#[derive(Debug, Clone)]
pub struct ActIfElse {
    pub cond: Conditional,
    pub yes: ActExpr,
    pub no: Option<ActExpr>,
}

#[derive(Debug, Clone)]
pub struct RawAction {
    pub ident: Ident,
    pub expr: Expression,
}

// NORMAL PARSE TREE

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Point {
    pub x: Expression,
    pub y: Expression,
}

#[derive(Debug, Clone)]
pub struct ListLiteral {
    pub parts: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ListRange {
    pub before: Vec<Expression>,
    pub after: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ListComprehension {
    pub expr: Expression,
    pub statements: Vec<VariableDef>,
}

#[derive(Debug, Clone)]
pub enum ListContents {
    Literal(ListLiteral),
    Range(ListRange),
    Comprehension(ListComprehension),
}

#[derive(Debug, Clone)]
pub struct IfElse {
    pub cond: Conditional,
    pub branches: Option<IfElseBranches>,
}

#[derive(Debug, Clone)]
pub struct IfElseBranches {
    pub yes: Expression,
    pub no: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub conds: Vec<OneConditional>,
}

#[derive(Debug, Clone)]
pub enum OneConditional {
    Equality(Equality),
    Inequality(Inequality),
}

#[derive(Debug, Clone)]
pub struct Equality {
    pub exprs: Vec<Expression>,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum InequalityType {
    LessThan,
    LessOrEqual,
    MoreThan,
    MoreOrEqual,
}

impl InequalityType {
    pub fn from_punct(punctuation: &Punctuation) -> Option<Self> {
        Some(match punctuation {
            Punctuation::LessThan => Self::LessThan,
            Punctuation::MoreThan => Self::MoreThan,
            Punctuation::LessOrEqual => Self::LessOrEqual,
            Punctuation::MoreOrEqual => Self::MoreOrEqual,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Inequality {
    pub exprs: Vec<Expression>,
    pub kinds: Vec<InequalityType>,
}

#[derive(Debug, Clone)]
pub struct AbsoluteValue {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Fraction {
    pub top: Expression,
    pub bottom: Expression,
}

#[derive(Debug, Clone)]
pub struct Root {
    pub nth: f64,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Ident,
    pub prime_count: u32,
    pub params: Vec<Expression>,
    pub power: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum EverythingElse {
    Grouping(Grouping),
    Point(Point),
    List(ListContents),
    IfElse(IfElse),
    Abs(AbsoluteValue),
    Fraction(Fraction),
    Root(Root),
    Call(FunctionCall),
    Number(Number),
    Ident(Ident),
    Differentiate(Differentiate),
    SumProd(SumProd),
}

#[derive(Debug, Clone)]
pub struct Power {
    pub base: Box<PostfixOrBelow>,
    pub power: Expression,
}

#[derive(Debug, Clone)]
pub struct ListIndexing {
    pub list: Box<PostfixOrBelow>,
    pub index: Expression,
}

#[derive(Debug, Clone)]
pub struct ListFiltering {
    pub list: Box<PostfixOrBelow>,
    pub filter: Conditional,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Element {
    X,
    Y,
}
#[derive(Debug, Clone)]
pub struct ElementAccess {
    pub expr: Box<PostfixOrBelow>,
    pub element: Element,
}

#[derive(Debug, Clone)]
pub enum PostfixOrBelow {
    Below(EverythingElse),
    Power(Power),
    Indexing(ListIndexing),
    Filtering(ListFiltering),
    Element(ElementAccess),
}

#[derive(Debug, Clone)]
pub struct Multiply {
    pub exprs: Vec<PostfixOrBelow>,
}

#[derive(Debug, Clone)]
pub enum MultiplyOrBelow {
    Below(PostfixOrBelow),
    Multiply(Multiply),
}

#[derive(Debug, Clone)]
pub struct Differentiate {
    pub expr: Box<MultiplyOrBelow>,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum SumOrProduct {
    Sum,
    Product,
}

#[derive(Debug, Clone)]
pub struct SumProd {
    pub kind: SumOrProduct,
    pub expr: Box<MultiplyOrBelow>,
    pub counter: Ident,
    pub from: Expression,
    pub to: Expression,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum AddOrSub {
    Add,
    Sub,
}

impl AddOrSub {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Punct(Punctuation::Add) => Some(AddOrSub::Add),
            Token::Punct(Punctuation::Sub) => Some(AddOrSub::Sub),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AddSub {
    pub types: Vec<AddOrSub>,
    pub exprs: Vec<MultiplyOrBelow>,
}

#[derive(Debug, Clone)]
pub enum Everything {
    AddSub(AddSub),
    Below(MultiplyOrBelow),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: Box<Everything>,
}
