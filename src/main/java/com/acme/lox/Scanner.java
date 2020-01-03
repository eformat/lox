package com.acme.lox;

import static com.acme.lox.TokenType.AND;
import static com.acme.lox.TokenType.BANG;
import static com.acme.lox.TokenType.BANG_EQUAL;
import static com.acme.lox.TokenType.CLASS;
import static com.acme.lox.TokenType.COMMA;
import static com.acme.lox.TokenType.DOT;
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
import static com.acme.lox.TokenType.SUPER;
import static com.acme.lox.TokenType.THIS;
import static com.acme.lox.TokenType.TRUE;
import static com.acme.lox.TokenType.VAR;
import static com.acme.lox.TokenType.WHILE;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Scanner {
    private final String source;
    private final List<Token> tokens = new ArrayList<>();
    private int start = 0;
    private int current = 0;
    private int line = 1;

    private static final Map<String, TokenType> keywords;

    static {
        keywords = new HashMap<>();
        keywords.put("and", AND);
        keywords.put("class", CLASS);
        keywords.put("else", ELSE);
        keywords.put("false", FALSE);
        keywords.put("for", FOR);
        keywords.put("fun", FUN);
        keywords.put("if", IF);
        keywords.put("nil", NIL);
        keywords.put("or", OR);
        keywords.put("print", PRINT);
        keywords.put("return", RETURN);
        keywords.put("super", SUPER);
        keywords.put("this", THIS);
        keywords.put("true", TRUE);
        keywords.put("var", VAR);
        keywords.put("while", WHILE);
    }

    Scanner(String source) {
        this.source = source;
    }

    List<Token> scanTokens() {
        while (!isAtEnd()) {
            // We are at the beginning of the next lexeme.
            start = current;
            scanToken();
        }

        tokens.add(new Token(EOF, "", null, line));
        return tokens;
    }

    private void scanToken() {
        char c = advance();
        switch (c) {
            case '(':
                addToken(LEFT_PAREN);
                break;
            case ')':
                addToken(RIGHT_PAREN);
                break;
            case '{':
                addToken(LEFT_BRACE);
                break;
            case '}':
                addToken(RIGHT_BRACE);
                break;
            case ',':
                addToken(COMMA);
                break;
            case '.':
                addToken(DOT);
                break;
            case '-':
                addToken(MINUS);
                break;
            case '+':
                addToken(PLUS);
                break;
            case ';':
                addToken(SEMICOLON);
                break;
            case '*':
                addToken(STAR);
                break;
            case '!':
                addToken(match('=') ? BANG_EQUAL : BANG);
                break;
            case '=':
                addToken(match('=') ? EQUAL_EQUAL : EQUAL);
                break;
            case '<':
                addToken(match('=') ? LESS_EQUAL : LESS);
                break;
            case '>':
                addToken(match('=') ? GREATER_EQUAL : GREATER);
                break;
            case '/':
                if (match('/')) {
                    // A comment goes until the end of the line.
                    while (peek() != '\n' && !isAtEnd())
                        advance();
                } else {
                    addToken(SLASH);
                }
                break;
            case ' ':
            case '\r':
            case '\t':
                // Ignore whitespace.
                break;

            case '\n':
                line++;
                break;
            case '"':
                string();
                break;
            default:
                if (isDigit(c)) {
                    number();
                } else if (isAlpha(c)) {
                    identifier();
                } else {
                    Lox.error(line, "Unexpected character.");
                }
                break;
        }
    }

    private char advance() {
        current++;
        return source.charAt(current - 1);
    }

    private void addToken(TokenType type) {
        addToken(type, null);
    }

    private void addToken(TokenType type, Object literal) {
        String text = source.substring(start, current);
        tokens.add(new Token(type, text, literal, line));
    }

    private boolean match(char expected) {
        if (isAtEnd())
            return false;
        if (source.charAt(current) != expected)
            return false;

        current++;
        return true;
    }

    private char peek() {
        if (isAtEnd())
            return '\0';
        return source.charAt(current);
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    private void string() {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n')
                line++;
            advance();
        }

        // Unterminated string.
        if (isAtEnd()) {
            Lox.error(line, "Unterminated string.");
            return;
        }

        // The closing ".
        advance();

        // Trim the surrounding quotes.
        String value = source.substring(start + 1, current - 1);
        addToken(STRING, value);
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private void number() {
        while (isDigit(peek()))
            advance();

        // Look for a fractional part.
        if (peek() == '.' && isDigit(peekNext())) {
            // Consume the "."
            advance();

            while (isDigit(peek()))
                advance();
        }

        addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private char peekNext() {
        if (current + 1 >= source.length())
            return '\0';
        return source.charAt(current + 1);
    }

    private void identifier() {
        while (isAlphaNumeric(peek()))
            advance();

        // See if the identifier is a reserved word.
        String text = source.substring(start, current);

        TokenType type = keywords.get(text);
        if (type == null)
            type = IDENTIFIER;
        addToken(type);
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }
}
