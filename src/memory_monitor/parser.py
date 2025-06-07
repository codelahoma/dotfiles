"""
SQL-like Query Parser for Memory Monitor.

Converts SQL-like query strings into Abstract Syntax Trees (AST)
for execution against FlowLoom memory data.
"""

import re
from dataclasses import dataclass
from typing import List, Optional, Union, Any, Dict, Tuple
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class TokenType(Enum):
    # Keywords
    SELECT = "SELECT"
    FROM = "FROM"
    WHERE = "WHERE"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    IN = "IN"
    BETWEEN = "BETWEEN"
    CONTAINS = "CONTAINS"
    MATCH = "MATCH"
    RELATED_TO = "RELATED_TO"
    HAS_RELATION = "HAS_RELATION"
    ORDER_BY = "ORDER BY"
    GROUP_BY = "GROUP BY"
    LIMIT = "LIMIT"
    OFFSET = "OFFSET"
    ASC = "ASC"
    DESC = "DESC"
    
    # Operators
    EQUALS = "="
    NOT_EQUALS = "!="
    LESS_THAN = "<"
    GREATER_THAN = ">"
    LESS_EQUAL = "<="
    GREATER_EQUAL = ">="
    LIKE = "LIKE"
    
    # Punctuation
    LPAREN = "("
    RPAREN = ")"
    COMMA = ","
    SEMICOLON = ";"
    DOT = "."
    STAR = "*"
    
    # Literals
    IDENTIFIER = "IDENTIFIER"
    STRING = "STRING"
    NUMBER = "NUMBER"
    BOOLEAN = "BOOLEAN"
    
    # Special
    EOF = "EOF"
    WHITESPACE = "WHITESPACE"


@dataclass
class Token:
    type: TokenType
    value: Any
    position: int
    length: int = 1


class LexerError(Exception):
    """Raised when lexer encounters invalid input."""
    pass


class Lexer:
    """Tokenize SQL-like query strings."""
    
    KEYWORDS = {
        'SELECT': TokenType.SELECT,
        'FROM': TokenType.FROM,
        'WHERE': TokenType.WHERE,
        'AND': TokenType.AND,
        'OR': TokenType.OR,
        'NOT': TokenType.NOT,
        'IN': TokenType.IN,
        'BETWEEN': TokenType.BETWEEN,
        'CONTAINS': TokenType.CONTAINS,
        'MATCH': TokenType.MATCH,
        'RELATED_TO': TokenType.RELATED_TO,
        'HAS_RELATION': TokenType.HAS_RELATION,
        'ORDER': TokenType.ORDER_BY,  # Will handle "ORDER BY" specially
        'GROUP': TokenType.GROUP_BY,  # Will handle "GROUP BY" specially
        'LIMIT': TokenType.LIMIT,
        'OFFSET': TokenType.OFFSET,
        'ASC': TokenType.ASC,
        'DESC': TokenType.DESC,
        'TRUE': TokenType.BOOLEAN,
        'FALSE': TokenType.BOOLEAN,
    }
    
    OPERATORS = {
        '=': TokenType.EQUALS,
        '!=': TokenType.NOT_EQUALS,
        '<>': TokenType.NOT_EQUALS,
        '<': TokenType.LESS_THAN,
        '>': TokenType.GREATER_THAN,
        '<=': TokenType.LESS_EQUAL,
        '>=': TokenType.GREATER_EQUAL,
    }
    
    def __init__(self, query: str):
        self.query = query.strip()
        self.position = 0
        self.tokens = []
        
    def tokenize(self) -> List[Token]:
        """Convert query string to tokens."""
        while self.position < len(self.query):
            self._skip_whitespace()
            
            if self.position >= len(self.query):
                break
                
            # Try different token types
            if self._match_string():
                continue
            elif self._match_number():
                continue
            elif self._match_operator():
                continue
            elif self._match_punctuation():
                continue
            elif self._match_keyword_or_identifier():
                continue
            else:
                char = self.query[self.position]
                raise LexerError(f"Unexpected character '{char}' at position {self.position}")
        
        # Add EOF token
        self.tokens.append(Token(TokenType.EOF, None, self.position))
        return self.tokens
    
    def _skip_whitespace(self):
        """Skip whitespace characters."""
        while (self.position < len(self.query) and 
               self.query[self.position].isspace()):
            self.position += 1
    
    def _peek(self, offset: int = 0) -> Optional[str]:
        """Peek at character at position + offset."""
        pos = self.position + offset
        if pos < len(self.query):
            return self.query[pos]
        return None
    
    def _match_string(self) -> bool:
        """Match quoted string literals."""
        if self._peek() not in ('"', "'"):
            return False
        
        quote_char = self.query[self.position]
        start_pos = self.position
        self.position += 1  # Skip opening quote
        
        value = ""
        while self.position < len(self.query):
            char = self.query[self.position]
            
            if char == quote_char:
                # End of string
                self.position += 1  # Skip closing quote
                self.tokens.append(Token(
                    TokenType.STRING, 
                    value, 
                    start_pos,
                    self.position - start_pos
                ))
                return True
            elif char == '\\' and self.position + 1 < len(self.query):
                # Escape sequence
                self.position += 1
                next_char = self.query[self.position]
                if next_char in ('"', "'", '\\', 'n', 't', 'r'):
                    if next_char == 'n':
                        value += '\n'
                    elif next_char == 't':
                        value += '\t'
                    elif next_char == 'r':
                        value += '\r'
                    else:
                        value += next_char
                else:
                    value += next_char
                self.position += 1
            else:
                value += char
                self.position += 1
        
        raise LexerError(f"Unterminated string starting at position {start_pos}")
    
    def _match_number(self) -> bool:
        """Match numeric literals."""
        if not (self._peek() and (self._peek().isdigit() or self._peek() == '.')):
            return False
        
        start_pos = self.position
        value = ""
        has_dot = False
        
        while self.position < len(self.query):
            char = self.query[self.position]
            
            if char.isdigit():
                value += char
                self.position += 1
            elif char == '.' and not has_dot:
                has_dot = True
                value += char
                self.position += 1
            else:
                break
        
        if value and value != '.':
            numeric_value = float(value) if has_dot else int(value)
            self.tokens.append(Token(
                TokenType.NUMBER,
                numeric_value,
                start_pos,
                self.position - start_pos
            ))
            return True
        
        return False
    
    def _match_operator(self) -> bool:
        """Match operators."""
        # Try two-character operators first
        two_char = self.query[self.position:self.position + 2]
        if two_char in self.OPERATORS:
            self.tokens.append(Token(
                self.OPERATORS[two_char],
                two_char,
                self.position,
                2
            ))
            self.position += 2
            return True
        
        # Try single-character operators
        one_char = self.query[self.position]
        if one_char in self.OPERATORS:
            self.tokens.append(Token(
                self.OPERATORS[one_char],
                one_char,
                self.position,
                1
            ))
            self.position += 1
            return True
        
        return False
    
    def _match_punctuation(self) -> bool:
        """Match punctuation characters."""
        char = self._peek()
        punctuation_map = {
            '(': TokenType.LPAREN,
            ')': TokenType.RPAREN,
            ',': TokenType.COMMA,
            ';': TokenType.SEMICOLON,
            '.': TokenType.DOT,
            '*': TokenType.STAR,
        }
        
        if char in punctuation_map:
            self.tokens.append(Token(
                punctuation_map[char],
                char,
                self.position,
                1
            ))
            self.position += 1
            return True
        
        return False
    
    def _match_keyword_or_identifier(self) -> bool:
        """Match keywords or identifiers."""
        if not (self._peek() and (self._peek().isalpha() or self._peek() == '_')):
            return False
        
        start_pos = self.position
        value = ""
        
        while (self.position < len(self.query) and
               (self.query[self.position].isalnum() or 
                self.query[self.position] in ('_', '$'))):
            value += self.query[self.position]
            self.position += 1
        
        # Check for special multi-word keywords
        if value.upper() in ('ORDER', 'GROUP'):
            # Look ahead for BY
            saved_pos = self.position
            self._skip_whitespace()
            
            if (self.position + 2 <= len(self.query) and
                self.query[self.position:self.position + 2].upper() == 'BY'):
                # Consume BY
                self.position += 2
                token_type = TokenType.ORDER_BY if value.upper() == 'ORDER' else TokenType.GROUP_BY
                self.tokens.append(Token(
                    token_type,
                    value.upper() + ' BY',
                    start_pos,
                    self.position - start_pos
                ))
                return True
            else:
                # Restore position
                self.position = saved_pos
        
        # Check if it's a keyword
        upper_value = value.upper()
        if upper_value in self.KEYWORDS:
            token_type = self.KEYWORDS[upper_value]
            token_value = value if token_type == TokenType.BOOLEAN else upper_value
            self.tokens.append(Token(
                token_type,
                token_value,
                start_pos,
                len(value)
            ))
        else:
            # It's an identifier
            self.tokens.append(Token(
                TokenType.IDENTIFIER,
                value,
                start_pos,
                len(value)
            ))
        
        return True


# AST Node Classes
@dataclass
class ASTNode:
    """Base class for AST nodes."""
    pass


@dataclass
class SelectClause(ASTNode):
    columns: List[str]


@dataclass
class FromClause(ASTNode):
    table: str


@dataclass
class Condition(ASTNode):
    """Base class for WHERE conditions."""
    pass


@dataclass
class ComparisonCondition(Condition):
    field: str
    operator: str
    value: Any


@dataclass
class ContainsCondition(Condition):
    field: str
    value: str


@dataclass
class MatchCondition(Condition):
    field: str
    pattern: str


@dataclass
class InCondition(Condition):
    field: str
    values: List[Any]


@dataclass
class BetweenCondition(Condition):
    field: str
    start: Any
    end: Any


@dataclass
class LogicalCondition(Condition):
    operator: str  # AND, OR
    left: Condition
    right: Condition


@dataclass
class NotCondition(Condition):
    condition: Condition


@dataclass
class FunctionCondition(Condition):
    function: str
    args: List[Any]


@dataclass
class WhereClause(ASTNode):
    condition: Condition


@dataclass
class OrderByClause(ASTNode):
    fields: List[Tuple[str, str]]  # [(field, direction)]


@dataclass
class LimitClause(ASTNode):
    limit: int
    offset: Optional[int] = None


@dataclass
class QueryAST(ASTNode):
    select: SelectClause
    from_clause: FromClause
    where: Optional[WhereClause] = None
    order_by: Optional[OrderByClause] = None
    limit: Optional[LimitClause] = None


class ParseError(Exception):
    """Raised when parser encounters invalid syntax."""
    pass


class Parser:
    """Parse tokens into AST."""
    
    def __init__(self):
        self.tokens = []
        self.position = 0
    
    def parse(self, tokens: List[Token]) -> QueryAST:
        """Parse tokens into QueryAST."""
        self.tokens = tokens
        self.position = 0
        
        try:
            ast = QueryAST(
                select=self._parse_select(),
                from_clause=self._parse_from()
            )
            
            # Optional clauses
            while not self._is_at_end():
                if self._current_token_is(TokenType.WHERE):
                    ast.where = self._parse_where()
                elif self._current_token_is(TokenType.ORDER_BY):
                    ast.order_by = self._parse_order_by()
                elif self._current_token_is(TokenType.LIMIT):
                    ast.limit = self._parse_limit()
                else:
                    break
            
            return ast
            
        except IndexError:
            raise ParseError("Unexpected end of query")
    
    def _current_token(self) -> Token:
        """Get current token."""
        if self.position < len(self.tokens):
            return self.tokens[self.position]
        return Token(TokenType.EOF, None, len(self.tokens))
    
    def _peek_token(self, offset: int = 1) -> Token:
        """Peek at token at current position + offset."""
        pos = self.position + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return Token(TokenType.EOF, None, len(self.tokens))
    
    def _current_token_is(self, token_type: TokenType) -> bool:
        """Check if current token is of given type."""
        return self._current_token().type == token_type
    
    def _consume(self, expected_type: TokenType) -> Token:
        """Consume token of expected type."""
        token = self._current_token()
        if token.type != expected_type:
            raise ParseError(f"Expected {expected_type}, got {token.type} at position {token.position}")
        self.position += 1
        return token
    
    def _advance(self) -> Token:
        """Advance to next token."""
        token = self._current_token()
        if not self._is_at_end():
            self.position += 1
        return token
    
    def _is_at_end(self) -> bool:
        """Check if at end of tokens."""
        return self._current_token().type == TokenType.EOF
    
    def _parse_select(self) -> SelectClause:
        """Parse SELECT clause."""
        self._consume(TokenType.SELECT)
        
        columns = []
        
        if self._current_token_is(TokenType.STAR):
            self._advance()
            columns.append("*")
        else:
            # Parse column list
            columns.append(self._consume(TokenType.IDENTIFIER).value)
            
            while self._current_token_is(TokenType.COMMA):
                self._advance()  # consume comma
                if self._current_token_is(TokenType.STAR):
                    self._advance()
                    columns.append("*")
                else:
                    columns.append(self._consume(TokenType.IDENTIFIER).value)
        
        return SelectClause(columns=columns)
    
    def _parse_from(self) -> FromClause:
        """Parse FROM clause."""
        self._consume(TokenType.FROM)
        table = self._consume(TokenType.IDENTIFIER).value
        return FromClause(table=table)
    
    def _parse_where(self) -> WhereClause:
        """Parse WHERE clause."""
        self._consume(TokenType.WHERE)
        condition = self._parse_condition()
        return WhereClause(condition=condition)
    
    def _parse_condition(self) -> Condition:
        """Parse condition with operator precedence."""
        return self._parse_or_condition()
    
    def _parse_or_condition(self) -> Condition:
        """Parse OR conditions (lowest precedence)."""
        left = self._parse_and_condition()
        
        while self._current_token_is(TokenType.OR):
            self._advance()  # consume OR
            right = self._parse_and_condition()
            left = LogicalCondition(operator="OR", left=left, right=right)
        
        return left
    
    def _parse_and_condition(self) -> Condition:
        """Parse AND conditions."""
        left = self._parse_not_condition()
        
        while self._current_token_is(TokenType.AND):
            self._advance()  # consume AND
            right = self._parse_not_condition()
            left = LogicalCondition(operator="AND", left=left, right=right)
        
        return left
    
    def _parse_not_condition(self) -> Condition:
        """Parse NOT conditions."""
        if self._current_token_is(TokenType.NOT):
            self._advance()  # consume NOT
            condition = self._parse_primary_condition()
            return NotCondition(condition=condition)
        
        return self._parse_primary_condition()
    
    def _parse_primary_condition(self) -> Condition:
        """Parse primary conditions."""
        if self._current_token_is(TokenType.LPAREN):
            # Parenthesized condition
            self._advance()  # consume (
            condition = self._parse_condition()
            self._consume(TokenType.RPAREN)
            return condition
        
        # Field-based condition
        field = self._consume(TokenType.IDENTIFIER).value
        
        if self._current_token_is(TokenType.CONTAINS):
            self._advance()
            value = self._parse_value()
            return ContainsCondition(field=field, value=value)
        
        elif self._current_token_is(TokenType.MATCH):
            self._advance()
            pattern = self._parse_value()
            return MatchCondition(field=field, pattern=pattern)
        
        elif self._current_token_is(TokenType.IN):
            self._advance()
            self._consume(TokenType.LPAREN)
            
            values = []
            values.append(self._parse_value())
            
            while self._current_token_is(TokenType.COMMA):
                self._advance()
                values.append(self._parse_value())
            
            self._consume(TokenType.RPAREN)
            return InCondition(field=field, values=values)
        
        elif self._current_token_is(TokenType.BETWEEN):
            self._advance()
            start = self._parse_value()
            self._consume(TokenType.AND)
            end = self._parse_value()
            return BetweenCondition(field=field, start=start, end=end)
        
        else:
            # Comparison operator
            operator_token = self._current_token()
            if operator_token.type in (TokenType.EQUALS, TokenType.NOT_EQUALS,
                                     TokenType.LESS_THAN, TokenType.GREATER_THAN,
                                     TokenType.LESS_EQUAL, TokenType.GREATER_EQUAL):
                self._advance()
                value = self._parse_value()
                return ComparisonCondition(field=field, operator=operator_token.value, value=value)
            else:
                raise ParseError(f"Expected comparison operator after field '{field}'")
    
    def _parse_value(self) -> Any:
        """Parse a value (string, number, or boolean)."""
        token = self._current_token()
        
        if token.type == TokenType.STRING:
            self._advance()
            return token.value
        elif token.type == TokenType.NUMBER:
            self._advance()
            return token.value
        elif token.type == TokenType.BOOLEAN:
            self._advance()
            return token.value.upper() == 'TRUE'
        else:
            raise ParseError(f"Expected value, got {token.type}")
    
    def _parse_order_by(self) -> OrderByClause:
        """Parse ORDER BY clause."""
        self._consume(TokenType.ORDER_BY)
        
        fields = []
        
        # Parse first field
        field = self._consume(TokenType.IDENTIFIER).value
        direction = "ASC"  # default
        
        if self._current_token_is(TokenType.ASC):
            self._advance()
            direction = "ASC"
        elif self._current_token_is(TokenType.DESC):
            self._advance()
            direction = "DESC"
        
        fields.append((field, direction))
        
        # Parse additional fields
        while self._current_token_is(TokenType.COMMA):
            self._advance()  # consume comma
            field = self._consume(TokenType.IDENTIFIER).value
            direction = "ASC"  # default
            
            if self._current_token_is(TokenType.ASC):
                self._advance()
                direction = "ASC"
            elif self._current_token_is(TokenType.DESC):
                self._advance()
                direction = "DESC"
            
            fields.append((field, direction))
        
        return OrderByClause(fields=fields)
    
    def _parse_limit(self) -> LimitClause:
        """Parse LIMIT clause."""
        self._consume(TokenType.LIMIT)
        limit = self._consume(TokenType.NUMBER).value
        
        offset = None
        if self._current_token_is(TokenType.OFFSET):
            self._advance()
            offset = self._consume(TokenType.NUMBER).value
        
        return LimitClause(limit=limit, offset=offset)


# Example usage and testing
if __name__ == "__main__":
    # Test queries
    test_queries = [
        "SELECT * FROM entities",
        "SELECT name, entityType FROM entities WHERE entityType = 'Task'",
        "SELECT * FROM entities WHERE observations CONTAINS 'status:active'",
        "SELECT * FROM entities WHERE entityType IN ('Task', 'WorkItem') AND status = 'active'",
        "SELECT * FROM entities WHERE created BETWEEN '2025-05-28' AND '2025-05-29' ORDER BY created DESC",
        "SELECT * FROM entities WHERE entityType = 'Task' ORDER BY created DESC LIMIT 10 OFFSET 5"
    ]
    
    for query in test_queries:
        print(f"\nQuery: {query}")
        try:
            lexer = Lexer(query)
            tokens = lexer.tokenize()
            print(f"Tokens: {[(t.type.name, t.value) for t in tokens if t.type != TokenType.EOF]}")
            
            parser = Parser()
            ast = parser.parse(tokens)
            print(f"AST: {ast}")
            
        except (LexerError, ParseError) as e:
            print(f"Error: {e}")