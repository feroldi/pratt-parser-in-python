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
    STAR = auto()
    SLASH = auto()
    CARET = auto()
    TILDE = auto()
    BANG = auto()
    QUESTION = auto()
    COLON = auto()
    NAME = auto()
    EOF = auto()


def punctuator(tok_kind):
    tok_to_str = {
        TokenKind.LEFT_PAREN: "(",
        TokenKind.RIGHT_PAREN: ")",
        TokenKind.COMMA: ",",
        TokenKind.ASSIGN: "=",
        TokenKind.PLUS: "+",
        TokenKind.MINUS: "-",
        TokenKind.STAR: "*",
        TokenKind.SLASH: "/",
        TokenKind.CARET: "^",
        TokenKind.TILDE: "~",
        TokenKind.BANG: "!",
        TokenKind.QUESTION: "?",
        TokenKind.COLON: ":",
    }
    return tok_to_str.get(tok_kind)


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
    (TokenKind.STAR, r"\*"),
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


class Precedence(IntEnum):
    ASSIGNMENT = auto()
    CONDITIONAL = auto()
    SUM = auto()
    PRODUCT = auto()
    EXPONENT = auto()
    PREFIX = auto()
    POSTFIX = auto()
    CALL = auto()


class Parser:
    def __init__(self, tokens, prefix_parsers, infix_parsers):
        self.tokens = tokens
        self.tok_buffer = []
        self.prefix_parsers = prefix_parsers
        self.infix_parsers = infix_parsers

    def parse_expr(self, precedence=0):
        tok = self.consume()
        prefix_parser = self.prefix_parsers.get(tok.kind)
        if prefix_parser:
            lhs = prefix_parser.parse(self, tok)
            while precedence < self.precedence_for_current_infix():
                tok = self.consume()
                infix_parser = self.infix_parsers.get(tok.kind)
                lhs = infix_parser.parse(self, lhs, tok)
            return lhs
        else:
            raise RuntimeError(f"could not parse '{tok.kind.name}'")

    def precedence_for_current_infix(self):
        infix_parser = self.infix_parsers.get(self.look_ahead().kind)
        return infix_parser.get_precedence() if infix_parser else 0

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
                f"expected token '{expected.name}', but found '{tok.kind.name}'"
            )
        return self.tok_buffer.pop(0)

    def look_ahead(self, distance=0):
        while distance >= len(self.tok_buffer):
            tok = self.tokens.next_tok()
            self.tok_buffer.append(tok)
        return self.tok_buffer[distance]


# AST


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


@dataclass
class InfixOpExpr(Expr):
    op: TokenKind
    lhs: Expr
    rhs: Expr

    def dump(self):
        return "(" + self.lhs.dump() + punctuator(self.op) + self.rhs.dump() + ")"


@dataclass
class PostfixOpExpr(Expr):
    op: TokenKind
    lhs: Expr

    def dump(self):
        return "(" + self.lhs.dump() + punctuator(self.op) + ")"


@dataclass
class ConditionalExpr(Expr):
    condition: Expr
    then_arm: Expr
    else_arm: Expr

    def dump(self):
        return (
            "("
            + self.condition.dump()
            + "?"
            + self.then_arm.dump()
            + ":"
            + self.else_arm.dump()
            + ")"
        )


# Prefix operator parsers


class PrefixParser:
    def parse(self, parser: Parser, token: Token) -> Expr:
        pass


class NameParser(PrefixParser):
    def parse(self, parser, token):
        return NameExpr(value=token.lexeme)


class PrefixOpParser(PrefixParser):
    def __init__(self, precedence):
        self.precedence = precedence

    def parse(self, parser, token):
        operand = parser.parse_expr(self.precedence)
        return PrefixOpExpr(op=token.kind, rhs=operand)


class GroupParser(PrefixParser):
    def parse(self, parser, token):
        assert token.kind == TokenKind.LEFT_PAREN
        inner_expr = parser.parse_expr()
        parser.consume(expected=TokenKind.RIGHT_PAREN)
        return inner_expr


# Infix, postfix and mixfix operator parsers


# Also used for postfix and mixfix.
class InfixParser:
    def parse(self, parser: Parser, lhs: Expr, token: Token) -> Expr:
        pass

    def get_precedence(self) -> Precedence:
        pass


class InfixOpParser(InfixParser):
    def __init__(self, precedence, is_right_associative):
        self.precedence = precedence
        self.is_right_associative = is_right_associative

    def parse(self, parser, lhs, token):
        rhs = parser.parse_expr(
            self.precedence - (1 if self.is_right_associative else 0)
        )
        return InfixOpExpr(op=token.kind, lhs=lhs, rhs=rhs)

    def get_precedence(self):
        return self.precedence


class PostfixOpParser(InfixParser):
    def __init__(self, precedence):
        self.precedence = precedence

    def parse(self, parser, lhs, token):
        return PostfixOpExpr(op=token.kind, lhs=lhs)

    def get_precedence(self):
        return self.precedence


class ConditionalParser(InfixParser):
    def parse(self, parser, lhs, token):
        assert token.kind == TokenKind.QUESTION
        then_arm = parser.parse_expr()
        parser.consume(expected=TokenKind.COLON)
        else_arm = parser.parse_expr(self.get_precedence() - 1)
        return ConditionalExpr(condition=lhs, then_arm=then_arm, else_arm=else_arm)

    def get_precedence(self):
        return Precedence.CONDITIONAL


# Main

if __name__ == "__main__":
    lexer = Lexer(input())
    prefix_parsers = {
        TokenKind.NAME: NameParser(),
        TokenKind.PLUS: PrefixOpParser(Precedence.PREFIX),
        TokenKind.MINUS: PrefixOpParser(Precedence.PREFIX),
        TokenKind.TILDE: PrefixOpParser(Precedence.PREFIX),
        TokenKind.BANG: PrefixOpParser(Precedence.PREFIX),
        TokenKind.LEFT_PAREN: GroupParser(),
    }
    infix_parsers = {
        TokenKind.PLUS: InfixOpParser(Precedence.SUM, is_right_associative=False),
        TokenKind.MINUS: InfixOpParser(Precedence.SUM, is_right_associative=False),
        TokenKind.STAR: InfixOpParser(Precedence.PRODUCT, is_right_associative=False),
        TokenKind.SLASH: InfixOpParser(Precedence.PRODUCT, is_right_associative=False),
        TokenKind.CARET: InfixOpParser(Precedence.EXPONENT, is_right_associative=True),
        TokenKind.BANG: PostfixOpParser(Precedence.POSTFIX),
        TokenKind.QUESTION: ConditionalParser(),
    }
    parser = Parser(lexer, prefix_parsers, infix_parsers)
    print(parser.parse_expr().dump())
