# Desmos

## Variables

Desmos supports the following data types:

- Number (`f64`)
- 2D Point
- List of number / List of point
    - No nested lists

Things that can be assigned to variables but aren't actually data:

- Actions
- Functions

## Precedence Levels:

1. Everything else (ignores precedence)
    1. point
    2. list lit
    3. list range
    4. list comprehension
    5. if-else
    6. absolute value
    7. fraction
    8. roots
2. Postfix Adjacent Ops
    1. exponentiation/2 (right ignores precedence)
    2. indexing/2 (right ignores precedence)
    3. element access/1
3. Multiplication/2
4. Function calls/2 (right ignores precedence)
5. Prefix Ops
    1. sum/3
    2. product/3
    3. d/dx differentiation/1
6. Add / Sub

## Syntax

- Grouping: `(1 + 2)`
    - Precedence: N/A
    - Syntax: `"(" expr ")"`
- Point: `(1, 2)`
    - Precedence: N/A
    - Syntax: `"(" expr "," expr ")"`
- List literal: `[1, 2, 3]`
    - Precedence: N/A
    - Syntax: `"[" expr,*  "]"`
- List range: `[1,...,3]` or `[1,1.1,...,3]`
    - Precedence: N/A
    - Syntax: `"[" expr "," (expr ",")? "..." "," expr "]"`
- List comprehension: `[(x, y) for x=[2,3], y=[1,2]]`
    - Precedence: N/A
    - Syntax: `"[" expr "for" statement,* "]"`
- If-else: `{a > b: 1, 0}`
    - Precedence: N/A
    - Syntax: `"\{" conditional : expr ("," expr)? "\}"`
- Functional call (with prime differentiation): `f'(x)`
    - Precedence: N/A
    - Syntax: `identifier "'"* "(" expr,* ")"`
- Multiplication: `ab`
    - Precedence: 1
    - Syntax: `expr expr`
- Add / sub: `a + b`
    - Precedence: 3
    - Syntax: `expr ("+" | "-") expr`
- Exponentiation: `a ^ {b}`
    - Precedence: 0
    - Syntax: `expr "^" "{" expr "}"`
- List indexing: `a[1]`
    - Precedence: 0
    - Syntax: `expr "[" expr "]"`
- Element access: `a.x`
    - Precedence: 0
    - Syntax: `expr "." ("x" | "y")`
- Absolute Value: `|x|`
    - Precedence: N/A
    - Syntax: `"|" expr "|"`
- Fraction: `\frac{1}{2}`
    - Precedence: N/A
    - Syntax: `"\frac" "{" expr "}" "{" expr "}"`
- d/dx Differentiation: `\frac{d}{dx} x^{2}`
    - Precedence: 2
    - Syntax: `"\frac" "{" "d" "}" "{" "dx" "}" expr`
- Roots: `\sqrt{2}`
    - Precedence: N/A
    - Syntax: `"\sqrt" ( "[" number "]" )? "{" expr "}"`
- Sums / Products: `\sum_{n=1}^{10}n`
    - Precedence: 2
    - Syntax: `("\sum" | "\product") "_" "{" statement "}" "^" "{" expr "}" expr`

### Syntax referenced

- Conditional: `a > b`
    - Syntax: `expr=+ | expr("<"|">")+`
- Identifier: `a0`
    - Syntax: `[a-zA-Z] ("_" "{" .* "}")?`
- Statement: `x = 1`
    - Syntax: `identifier "=" expr`

### Sorted by starting item:

- "["
    - List literal
    - List range (differentiated by "...")
    - List comprehension (differentiated by "for")
- "("
    - Grouping
    - Point (differentiated by ",")
- expr
    - Multiplication
    - Add / sub (differentiated by "+" / "-")
    - Exponentiation (differentiated by "^")
    - List indexing (differentiated by "[")
    - Element access (differentiated by ".")
    - Conditional (differentiated by "=" / ">" / "<")
    - Statement (differentiated by "=" and location)
- "\{"
    - Conditional
- "|"
    - Absolute value
- "\frac"
    - Fraction
    - Differentiation (differentiated by "d" and "dx")
- "\sqrt"
- "\sum" | "\product"
