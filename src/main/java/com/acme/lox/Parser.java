package com.acme.lox;

import static com.acme.lox.TokenType.AND;
import static com.acme.lox.TokenType.BANG;
import static com.acme.lox.TokenType.BANG_EQUAL;
import static com.acme.lox.TokenType.COMMA;
import static com.acme.lox.TokenType.ELSE;
import static com.acme.lox.TokenType.EOF;
import static com.acme.lox.TokenType.EQUAL;
import static com.acme.lox.TokenType.EQUAL_EQUAL;
import static com.acme.lox.TokenType.FALSE;
import static com.acme.lox.TokenType.FOR;
import static com.acme.lox.TokenType.FUN;
import static com.acme.lox.TokenType.GREATER;
import static com.acme.lox.TokenType.GREATER_EQUAL;
import static com.acme.lox.TokenType.IDENTIFIER;
import static com.acme.lox.TokenType.IF;
import static com.acme.lox.TokenType.LEFT_BRACE;
import static com.acme.lox.TokenType.LEFT_PAREN;
import static com.acme.lox.TokenType.LESS;
import static com.acme.lox.TokenType.LESS_EQUAL;
import static com.acme.lox.TokenType.MINUS;
import static com.acme.lox.TokenType.NIL;
import static com.acme.lox.TokenType.NUMBER;
import static com.acme.lox.TokenType.OR;
import static com.acme.lox.TokenType.PLUS;
import static com.acme.lox.TokenType.PRINT;
import static com.acme.lox.TokenType.RETURN;
import static com.acme.lox.TokenType.RIGHT_BRACE;
import static com.acme.lox.TokenType.RIGHT_PAREN;
import static com.acme.lox.TokenType.SEMICOLON;
import static com.acme.lox.TokenType.SLASH;
import static com.acme.lox.TokenType.STAR;
import static com.acme.lox.TokenType.STRING;
import static com.acme.lox.TokenType.TRUE;
import static com.acme.lox.TokenType.VAR;
import static com.acme.lox.TokenType.WHILE;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/*
//@formatter:off

// Expressions

expression → literal
           | unary
           | binary
           | grouping ;

literal    → NUMBER | STRING | "false" | "true" | "nil" ;
grouping   → "(" expression ")" ;
unary      → ( "-" | "!" ) expression ;
binary     → expression operator expression ;
operator   → "==" | "!=" | "<" | "<=" | ">" | ">="
           | "+"  | "-"  | "*" | "/" ;

--

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "false" | "true" | "nil"
               | "(" expression ")" ;

--

// Statements

program   → statement* EOF ;

statement → exprStmt
          | printStmt ;

exprStmt  → expression ";" ;
printStmt → "print" expression ";" ;

// Statements and Declarations

program     → declaration* EOF ;

declaration → varDecl
            | statement ;

statement   → exprStmt
            | printStmt ;

varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;

primary → "true" | "false" | "nil"
        | NUMBER | STRING
        | "(" expression ")"
        | IDENTIFIER ;

--

// Assignment

expression → assignment ;
assignment → IDENTIFIER "=" assignment
           | equality ;

--

// Blocks

statement → exprStmt
          | printStmt
          | block ;

block     → "{" declaration* "}" ;

--

// Conditional Execution

statement → exprStmt
          | ifStmt
          | printStmt
          | block ;

ifStmt    → "if" "(" expression ")" statement ( "else" statement )? ;

--

// Logical Operators

expression → assignment ;
assignment → identifier "=" assignment
           | logic_or ;
logic_or   → logic_and ( "or" logic_and )* ;
logic_and  → equality ( "and" equality )* ;

--

// While Loops

statement → exprStmt
          | ifStmt
          | printStmt
          | whileStmt
          | block ;

whileStmt → "while" "(" expression ")" statement ;

--

// For Loops

statement → exprStmt
          | forStmt
          | ifStmt
          | printStmt
          | whileStmt
          | block ;

forStmt   → "for" "(" ( varDecl | exprStmt | ";" )
                      expression? ";"
                      expression? ")" statement ;

--

// Function Calls

unary → ( "!" | "-" ) unary | call ;
call  → primary ( "(" arguments? ")" )* ;

arguments → expression ( "," expression )* ;

--

// Function Declarations

declaration → funDecl
            | varDecl
            | statement ;

funDecl  → "fun" function ;
function → IDENTIFIER "(" parameters? ")" block ;            

--

// Return Statements

statement  → exprStmt
           | forStmt
           | ifStmt
           | printStmt
           | returnStmt
           | whileStmt
           | block ;

returnStmt → "return" expression? ";" ;


//@formatter:on
*/

class Parser {
    private final List<Token> tokens;
    private int current = 0;

    private static class ParseError extends RuntimeException {
    }

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    List<Stmt> parse() {
        List<Stmt> statements = new ArrayList<>();
        while (!isAtEnd()) {
            statements.add(declaration());
        }

        return statements;
    }

    private Expr expression() {
        return assignment();
    }

    private Expr assignment() {
        Expr expr = or();

        if (match(EQUAL)) {
            Token equals = previous();
            Expr value = assignment();

            if (expr instanceof Expr.Variable) {
                Token name = ((Expr.Variable) expr).name;
                return new Expr.Assign(name, value);
            }

            error(equals, "Invalid assignment target.");
        }

        return expr;
    }

    private Expr or() {
        Expr expr = and();

        while (match(OR)) {
            Token operator = previous();
            Expr right = and();
            expr = new Expr.Logical(expr, operator, right);
        }

        return expr;
    }

    private Expr and() {
        Expr expr = equality();

        while (match(AND)) {
            Token operator = previous();
            Expr right = equality();
            expr = new Expr.Logical(expr, operator, right);
        }

        return expr;
    }

    private Stmt declaration() {
        try {
            if (match(FUN))
                return function("function");
            if (match(VAR))
                return varDeclaration();

            return statement();
        } catch (ParseError error) {
            synchronize();
            return null;
        }
    }

    private Stmt.Function function(String kind) {
        Token name = consume(IDENTIFIER, "Expect " + kind + " name.");

        consume(LEFT_PAREN, "Expect '(' after " + kind + " name.");
        List<Token> parameters = new ArrayList<>();
        if (!check(RIGHT_PAREN)) {
            do {
                if (parameters.size() >= 255) {
                    error(peek(), "Cannot have more than 255 parameters.");
                }

                parameters.add(consume(IDENTIFIER, "Expect parameter name."));
            } while (match(COMMA));
        }
        consume(RIGHT_PAREN, "Expect ')' after parameters.");

        consume(LEFT_BRACE, "Expect '{' before " + kind + " body.");
        List<Stmt> body = block();
        return new Stmt.Function(name, parameters, body);
    }

    private Stmt statement() {
        if (match(FOR))
            return forStatement();

        if (match(IF))
            return ifStatement();

        if (match(PRINT))
            return printStatement();

        if (match(RETURN))
            return returnStatement();

        if (match(WHILE))
            return whileStatement();

        if (match(LEFT_BRACE))
            return new Stmt.Block(block());

        return expressionStatement();
    }

    private Stmt returnStatement() {
        Token keyword = previous();
        Expr value = null;
        if (!check(SEMICOLON)) {
            value = expression();
        }

        consume(SEMICOLON, "Expect ';' after return value.");
        return new Stmt.Return(keyword, value);
    }

    private Stmt forStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'for'.");

        Stmt initializer;
        if (match(SEMICOLON)) {
            initializer = null;
        } else if (match(VAR)) {
            initializer = varDeclaration();
        } else {
            initializer = expressionStatement();
        }

        Expr condition = null;
        if (!check(SEMICOLON)) {
            condition = expression();
        }
        consume(SEMICOLON, "Expect ';' after loop condition.");

        Expr increment = null;
        if (!check(RIGHT_PAREN)) {
            increment = expression();
        }
        consume(RIGHT_PAREN, "Expect ')' after for clauses.");

        Stmt body = statement();

        if (increment != null) {
            body = new Stmt.Block(Arrays.asList(body, new Stmt.Expression(increment)));
        }

        if (condition == null)
            condition = new Expr.Literal(true);
        body = new Stmt.While(condition, body);

        if (initializer != null) {
            body = new Stmt.Block(Arrays.asList(initializer, body));
        }

        return body;
    }

    private Stmt ifStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'if'.");
        Expr condition = expression();
        consume(RIGHT_PAREN, "Expect ')' after if condition.");
        Stmt thenBranch = statement();
        Stmt elseBranch = null;
        if (match(ELSE)) {
            elseBranch = statement();
        }

        return new Stmt.If(condition, thenBranch, elseBranch);
    }

    private Stmt expressionStatement() {
        Expr expr = expression();
        consume(SEMICOLON, "Expect ';' after expression.");
        return new Stmt.Expression(expr);
    }

    private List<Stmt> block() {
        List<Stmt> statements = new ArrayList<>();

        while (!check(RIGHT_BRACE) && !isAtEnd()) {
            statements.add(declaration());
        }

        consume(RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    private Stmt printStatement() {
        Expr value = expression();
        consume(SEMICOLON, "Expect ';' after value.");
        return new Stmt.Print(value);
    }

    private Stmt varDeclaration() {
        Token name = consume(IDENTIFIER, "Expect variable name.");

        Expr initializer = null;
        if (match(EQUAL)) {
            initializer = expression();
        }

        consume(SEMICOLON, "Expect ';' after variable declaration.");
        return new Stmt.Var(name, initializer);
    }

    private Stmt whileStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'while'.");
        Expr condition = expression();
        consume(RIGHT_PAREN, "Expect ')' after condition.");
        Stmt body = statement();

        return new Stmt.While(condition, body);
    }

    private Expr equality() {
        Expr expr = comparison();

        while (match(BANG_EQUAL, EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr comparison() {
        Expr expr = addition();

        while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            Token operator = previous();
            Expr right = addition();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr addition() {
        Expr expr = multiplication();

        while (match(MINUS, PLUS)) {
            Token operator = previous();
            Expr right = multiplication();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr multiplication() {
        Expr expr = unary();

        while (match(SLASH, STAR)) {
            Token operator = previous();
            Expr right = unary();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr unary() {
        if (match(BANG, MINUS)) {
            Token operator = previous();
            Expr right = unary();
            return new Expr.Unary(operator, right);
        }

        return call();
    }

    private Expr call() {
        Expr expr = primary();

        while (true) {
            if (match(LEFT_PAREN)) {
                expr = finishCall(expr);
            } else {
                break;
            }
        }

        return expr;
    }

    private Expr finishCall(Expr callee) {
        List<Expr> arguments = new ArrayList<>();
        if (!check(RIGHT_PAREN)) {
            do {
                if (arguments.size() >= 255) {
                    error(peek(), "Cannot have more than 255 arguments.");
                }
                arguments.add(expression());
            } while (match(COMMA));
        }

        Token paren = consume(RIGHT_PAREN, "Expect ')' after arguments.");

        return new Expr.Call(callee, paren, arguments);
    }

    private Expr primary() {
        if (match(FALSE))
            return new Expr.Literal(false);
        if (match(TRUE))
            return new Expr.Literal(true);
        if (match(NIL))
            return new Expr.Literal(null);

        if (match(NUMBER, STRING)) {
            return new Expr.Literal(previous().literal);
        }

        if (match(IDENTIFIER)) {
            return new Expr.Variable(previous());
        }

        if (match(LEFT_PAREN)) {
            Expr expr = expression();
            consume(RIGHT_PAREN, "Expect ')' after expression.");
            return new Expr.Grouping(expr);
        }

        throw error(peek(), "Expect expression.");
    }

    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    private Token consume(TokenType type, String message) {
        if (check(type))
            return advance();

        throw error(peek(), message);
    }

    private boolean check(TokenType type) {
        if (isAtEnd())
            return false;
        return peek().type == type;
    }

    private Token advance() {
        if (!isAtEnd())
            current++;
        return previous();
    }

    private boolean isAtEnd() {
        return peek().type == EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }

    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    private void synchronize() {
        advance();

        while (!isAtEnd()) {
            if (previous().type == SEMICOLON)
                return;

            switch (peek().type) {
                case CLASS:
                case FUN:
                case VAR:
                case FOR:
                case IF:
                case WHILE:
                case PRINT:
                case RETURN:
                    return;
            }

            advance();
        }
    }
}
