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
    }
    return tok_to_str.get(tok_kind)


class Parser:
    def __init__(self, tokens, prefix_parsers):
        self.tokens = tokens
        self.tok_buffer = []
        self.prefix_parsers = prefix_parsers

    def parse_expr(self):
        tok = self.consume()
        prefix_parser = self.prefix_parsers.get(tok.kind)
        if prefix_parser:
            return prefix_parser.parse(self, tok)
        else:
            raise RuntimeError(f"could not parse {tok.kind}")

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


class Expr:
    def dump(self) -> str:
        pass


@dataclass
class NameExpr(Expr):
    value: str

    def dump(self):
        return self.value

@dataclass
class PrefixOpExpr(Expr):
    op: TokenKind
    rhs: Expr

    def dump(self):
        return "(" + punctuator(self.op) + self.rhs.dump() + ")"


class PrefixParser:
    def parse(self, parser: Parser, token: Token) -> Expr:
        pass

class NameParser(PrefixParser):
    def parse(self, parser, token):
        return NameExpr(value=token.lexeme)


class PrefixOpParser(PrefixParser):
    def parse(self, parser, token):
        operand = parser.parse_expr()
        return PrefixOpExpr(op=token.kind, rhs=operand)


if __name__ == "__main__":
    lexer = Lexer(input())
    prefix_parsers = {
        TokenKind.NAME: NameParser(),
        TokenKind.PLUS: PrefixOpParser(),
        TokenKind.MINUS: PrefixOpParser(),
        TokenKind.TILDE: PrefixOpParser(),
        TokenKind.BANG: PrefixOpParser(),
    }
    parser = Parser(lexer, prefix_parsers)
    print(parser.parse_expr().dump())

