import re

from enum import IntEnum, auto
from dataclasses import dataclass


class TokenKind(IntEnum):
    LEFT_PAREN = auto()
    RIGHT_PAREN = auto()
    COMMA = auto()
    ASSIGN = auto()
    PLUS = auto()
    MINUS = auto()
    ASTERISK = auto()
    SLASH = auto()
    CARET = auto()
    TILDE = auto()
    BANG = auto()
    QUESTION = auto()
    COLON = auto()
    NAME = auto()
    EOF = auto()


@dataclass
class Token:
    kind: TokenKind
    lexeme: str


TOK_PATTERNS = [
    (TokenKind.LEFT_PAREN, r"\("),
    (TokenKind.RIGHT_PAREN, r"\)"),
    (TokenKind.COMMA, r","),
    (TokenKind.ASSIGN, r"="),
    (TokenKind.PLUS, r"\+"),
    (TokenKind.MINUS, r"-"),
    (TokenKind.ASTERISK, r"\*"),
    (TokenKind.SLASH, r"/"),
    (TokenKind.CARET, r"\^"),
    (TokenKind.TILDE, r"~"),
    (TokenKind.BANG, r"!"),
    (TokenKind.QUESTION, r"\?"),
    (TokenKind.COLON, r":"),
    (TokenKind.NAME, r"[A-Za-z][A-Za-z0-9]*"),
    (TokenKind.EOF, r""),
]


class Lexer:
    def __init__(self, chars):
        self.chars = chars

    def next_tok(self):
        self.chars = self.chars.lstrip()
        for tok_kind, pattern in TOK_PATTERNS:
            res = re.match(pattern, self.chars)
            if res:
                lexeme = self.chars[0 : res.end()]
                self.chars = self.chars[res.end() :]
                return Token(tok_kind, lexeme)
        return Token(TokenKind.EOF, "")

    def lex(self):
        tokens = []
        while True:
            tok = self.next_tok()
            tokens.append(tok)
            if tok.kind == TokenKind.EOF:
                break
        return tokens


def punctuator(tok_kind):
    tok_to_str = {
        TokenKind.LEFT_PAREN: "(",
        TokenKind.RIGHT_PAREN: ")",
        TokenKind.COMMA: ",",
        TokenKind.ASSIGN: "=",
        TokenKind.PLUS: "+",
        TokenKind.MINUS: "-",
        TokenKind.ASTERISK: "*",
        TokenKind.SLASH: "/",
        TokenKind.CARET: "^",
        TokenKind.TILDE: "~",
        TokenKind.BANG: "!",
        TokenKind.QUESTION: "?",
        TokenKind.COLON: ":",
        TokenKind.NAME: "[a-z]+",
    }
    return tok_to_str.get(tok_kind)


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_buffer = []

    def match(self, expected):
        tok = self.look_ahead()
        if tok.kind != expected:
            return False
        self.consume()
        return True

    def consume(self, expected=None):
        tok = self.look_ahead()
        if expected and tok.kind != expected:
            raise RuntimeError(
                f"expected token {expected.name}, but found {tok.kind.name}"
            )
        return self.tok_buffer.pop(0)

    def look_ahead(self, distance=0):
        while distance >= len(self.tok_buffer):
            tok = self.tokens.next_tok()
            self.tok_buffer.append(tok)
        return self.tok_buffer[distance]
