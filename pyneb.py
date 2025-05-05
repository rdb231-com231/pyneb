#######################################
# IMPORTS
#######################################

from string_with_arrows import *

import sys
import string
import unicodedata
import os

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS += 'áéíóúãâêôãõçÁÉÍÓÚÃÂÊÔÃÕÇ'	
LETTERS_DIGITS = LETTERS + DIGITS
FILENAME = ''

DEFAULT_CONFIG = {
	'version': "1.0.0",
	'author': 'mewplush',
	'interpreter': 'pyneb',
	'python': {
		'author-version':'3.12.10',
		'user-version': sys.version,
	},
	'description': 'PyNeb é o interpretador principal da linguagem Nebula. Escrito em Python, ele é capaz de executar scripts Nebula e também pode ser usado como um shell interativo.',
	'special-keys': {
		'__shell__': 'Nebula Shell',
		'.config': 'Configuração do PyNeb',
		'__path__': 'Caminho do arquivo'
	}
}

#######################################
# ERRORS
#######################################

class Error:
	def __init__(self, pos_start, pos_end, error_name, details):
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.error_name = error_name
		self.details = details
	
	def as_string(self):
		result  = f'{self.error_name}: {self.details}\n'
		result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Caractere Ilegal', details)

class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Syntaxe Inválida', details)

class CheckError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Error Conferindo Informações', details)

class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.context

		while ctx:
			result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent

		return 'Traceback (most recent call last):\n' + result

#######################################
# POSITION
#######################################

class Position:
	def __init__(self, idx, ln, col, fn, ftxt):
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt

	def advance(self, current_char=None):
		self.idx += 1
		self.col += 1

		if current_char == '\n':
			self.ln += 1
			self.col = 0

		return self

	def copy(self):
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

#######################################
# TOKENS
#######################################

TT_INT			= 'INT'
TT_STRING		= 'STRING'
TT_FLOAT    	= 'FLOAT'
TT_IDENTIFIER	= 'IDENTIFIER'
TT_KEYWORD		= 'KEYWORD'
TT_PLUS     	= 'PLUS'
TT_MINUS    	= 'MINUS'
TT_MUL      	= 'MUL'
TT_DIV      	= 'DIV'
TT_POW			= 'POW'
TT_EQ			= 'EQ'
TT_LPAREN   	= 'LPAREN'
TT_RPAREN   	= 'RPAREN'
TT_EOF			= 'EOF'
TT_GTE = 'GTE'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_LT = 'LT'
TT_NE = 'NE'
TT_EE = 'EE'
TT_ARROW = 'ARROW'
TT_SEMICOLON = 'SEMICOLON'
TT_COMMA = 'COMMA'
TT_QUOTE = 'QUOTE'
TT_LBRACKET = 'LBRACKET'
TT_RBRACKET = 'RBRACKET'
TT_NEWLINE = 'NEWLINE'

KEYWORDS = [
	'declarar',
	'constante',
	'assim',
	'também',
	'como',
	'ou',
	'e',
	'função',
	'retorne',
	'não',
	'se',
	'então',
	'senão',
	'mas',
	'enquanto',
	'caminhando',
	'até',
	'mudando',
	'faça',
	'igual',
	'a',
	'usando',
	'fim',
	'retornar',
	'continuar',
	'parar',
	'conferir',
	'levantar',
	'detalhes',
	'importar',
	'de'
]

class Token:
	def __init__(self, type_, value=None, pos_start=None, pos_end=None):
		self.type = type_
		self.value = value

		if pos_start:
			self.pos_start = pos_start.copy()
			self.pos_end = pos_start.copy()
			self.pos_end.advance()

		if pos_end:
			self.pos_end = pos_end.copy()

	def matches(self, type_, value):
		return self.type == type_ and self.value == value
	
	def __repr__(self):
		if self.value: return f'{self.type}:{self.value}'
		return f'{self.type}'

#######################################
# LEXER
#######################################

class Lexer:
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.current_char = None
		self.advance()
	
	def advance(self):
		self.pos.advance(self.current_char)
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

	def make_tokens(self):
		tokens = []

		while self.current_char != None:
			if self.current_char in ' \t':
				self.advance()
			elif self.current_char in ';\n':
				tokens.append(Token(TT_NEWLINE, pos_start=self.pos))
				self.advance()
			elif self.current_char == '#':
				self.skip_comment()
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			elif self.current_char in LETTERS:
				tokens.append(self.make_identifier())
			elif self.current_char == '+':
				tokens.append(Token(TT_PLUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '-':
				tokens.append(Token(TT_MINUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '*':
				tokens.append(Token(TT_MUL, pos_start=self.pos))
				self.advance()
			elif self.current_char == ';':
				tokens.append(Token(TT_SEMICOLON, pos_start=self.pos))
				self.advance()
			elif self.current_char == '/':
				tokens.append(Token(TT_DIV, pos_start=self.pos))
				self.advance()
			elif self.current_char == '^':
				tokens.append(Token(TT_POW, pos_start=self.pos))
				self.advance()
			elif self.current_char == '=':
				tokens.append(self.make_equals())
			elif self.current_char == '(':
				tokens.append(Token(TT_LPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_char == ')':
				tokens.append(Token(TT_RPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_char == '"':
				tokens.append(self.make_string())
			elif self.current_char == '!':
				tok, error = self.make_not_equals()
				if error: return [], error
				tokens.append(tok)
			elif self.current_char == '<':
				tokens.append(self.make_less_than())
			elif self.current_char == '>':
				tokens.append(self.make_greater_than())
			elif self.current_char == ',':
				tokens.append(Token(TT_COMMA, pos_start=self.pos))
				self.advance()
			elif self.current_char == '[':
				tokens.append(Token(TT_LBRACKET, pos_start=self.pos))
				self.advance()
			elif self.current_char == ']':
				tokens.append(Token(TT_RBRACKET, pos_start=self.pos))
				self.advance()
			else:
				pos_start = self.pos.copy()
				char = self.current_char
				self.advance()
				return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

		tokens.append(Token(TT_EOF, pos_start=self.pos))
		return tokens, None

	def make_number(self):
		num_str = ''
		dot_count = 0
		pos_start = self.pos.copy()

		while self.current_char != None and self.current_char in DIGITS + '.':
			if self.current_char == '.':
				if dot_count == 1: break
				dot_count += 1
			num_str += self.current_char
			self.advance()

		if dot_count == 0:
			return Token(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

	def make_identifier(self):
		id_str = ''
		pos_start = self.pos.copy()

		while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
			id_str += self.current_char
			self.advance()

		tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
		return Token(tok_type, id_str, pos_start, self.pos)
	
	def make_not_equals(self):
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
		else:
			self.pos = pos_start
			return None, IllegalCharError(pos_start, self.pos, "Expected '=' after '!'")
	
	def skip_comment(self):
		self.advance()

		while self.current_char != '\n':
			self.advance()

	def make_equals(self):
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			return Token(TT_EE, pos_start=pos_start, pos_end=self.pos)
		elif self.current_char == '>':
			self.advance()
			return Token(TT_ARROW, pos_start=pos_start, pos_end=self.pos)
		else:
			return Token(TT_EQ, pos_start=pos_start, pos_end=self.pos)

	def make_less_than(self):
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			return Token(TT_LTE, pos_start=pos_start, pos_end=self.pos)
		else:
			return Token(TT_LT, pos_start=pos_start, pos_end=self.pos)
	
	def make_greater_than(self):
		pos_start = self.pos.copy()
		self.advance()

		if self.current_char == '=':
			self.advance()
			return Token(TT_GTE, pos_start=pos_start, pos_end=self.pos)
		else:
			return Token(TT_GT, pos_start=pos_start, pos_end=self.pos)
	
	def make_string(self):
		string = ''
		pos_start = self.pos.copy()
		esc_char = False
		self.advance()
		
		esc_chars = {
			'n': '\n',
			't': '\t'
		}
		while self.current_char != None and (self.current_char != '"' or esc_char):
			if esc_char:
				string += esc_chars.get(self.current_char, self.current_char)
			else:
				if self.current_char == '\\':
					esc_char = True
					self.advance()
					continue
				string += self.current_char

			self.advance()
			esc_char = False
	
		self.advance()
		return Token(TT_STRING, string, pos_start, self.pos)

#######################################
# NODES
#######################################

class FuncDefNode:
	def __init__(self, var_name_tok, arg_name_toks, body_node, should_auto_return):
		self.var_name_tok = var_name_tok
		self.arg_name_toks = arg_name_toks
		self.body_node = body_node
		self.should_auto_return = should_auto_return

		if self.var_name_tok:
			self.pos_start = self.var_name_tok.pos_start
		elif len(self.arg_name_toks) > 0:
			self.pos_start = self.arg_name_toks[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start
		
		self.pos_end = self.body_node.pos_end

class CallNode:
	def __init__(self, node_to_call, arg_nodes):
		self.node_to_call = node_to_call
		self.arg_nodes = arg_nodes

		self.pos_start = self.node_to_call.pos_start

		if len(self.arg_nodes) > 0:
			self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
		else:
			self.pos_end = self.node_to_call.pos_end

class NumberNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		
		return f'{self.tok}'

class StringNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = tok.pos_start
		self.pos_end = tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

class ListNode:
	def __init__(self, element_nodes, pos_start=None, pos_end=None):
		self.element_nodes = element_nodes

		self.pos_start = pos_start if pos_start else self.element_nodes.pos_start
		self.pos_end = pos_end if pos_end else self.element_nodes.pos_end

class IfNode:
	def __init__(self, cases, else_case):
		self.cases = cases
		self.else_case = else_case

		self.pos_start = self.cases[0][0].pos_start
		self.pos_end = (self.else_case or self.cases[-1])[0].pos_end if self.else_case or self.cases else None
	
	def __repr__(self):
		return f'IfNode({self.cases}, {self.else_case})'
	
class ForNode:
	def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body, should_return_null):
		self.var_name_tok = var_name_tok
		self.start_value_node = start_value_node
		self.end_value_node = end_value_node
		self.step_value_node = step_value_node
		self.body_node = body
		self.should_return_null = should_return_null

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.body_node.pos_end

class WhileNode:
	def __init__(self, condition_node, body_node, should_return_null):
		self.condition_node = condition_node
		self.body_node = body_node
		self.should_return_null = should_return_null

		self.pos_start = self.condition_node.pos_start
		self.pos_end = self.body_node.pos_end





class VarAccessNode:
	def __init__(self, var_name_tok):
		self.var_name_tok = var_name_tok

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.var_name_tok.pos_end

class VarAssignNode:
	def __init__(self, var_name_tok, value_node, constant=False):
		self.var_name_tok = var_name_tok
		self.value_node = value_node
		self.constant = constant

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.value_node.pos_end if self.value_node else self.var_name_tok.pos_end

class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end

	def __repr__(self):
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node

		self.pos_start = self.op_tok.pos_start
		self.pos_end = node.pos_end

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'

class ReturnNode:
	def __init__(self, node_to_return, pos_start, pos_end):
		self.node_to_return = node_to_return
		
		self.pos_start = pos_start
		self.pos_end = pos_end

class ContinueNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end

class BreakNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end

class CheckNode:
	def __init__(self, condition, error_to_raise, details, pos_start, pos_end):
		self.condition_node = condition
		self.error_name = error_to_raise
		self.details = details
		
		self.pos_start = pos_start
		self.pos_end = pos_end

class RaiseNode:
	def __init__(self, error_name, details, pos_start, pos_end):
		self.error_name = error_name
		self.details = details
		
		self.pos_start = pos_start
		self.pos_end = pos_end

class ImportNode:
	def __init__(self, file_path, functions, pos_start, pos_end):
		self.file_path = file_path
		self.functions = functions
		
		self.pos_start = pos_start
		self.pos_end = pos_end

#######################################
# PARSE RESULT
#######################################

class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
		self.advance_count = 0
		self.to_reverse_count = 0

	def register_advancement(self):
		self.advance_count += 1

	def register(self, res):
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node
	
	def try_register(self, res):
		if res.error:
			self.to_reverse_count = res.advance_count
			return None
		return self.register(res)

	def success(self, node):
		self.node = node
		return self

	def failure(self, error):
		if not self.error or self.advance_count == 0:
			self.error = error
		return self

#######################################
# PARSER
#######################################

class Parser:
	def __init__(self, tokens):
		self.tokens = tokens
		self.tok_idx = -1
		self.stack = []
		self.advance()

	def advance(self, res=None):
		if res: res.register_advancement()
		self.tok_idx += 1
		self.update_current_tok()
		return self.current_tok
	
	def reverse(self, amount=1):
		self.tok_idx -= amount
		self.update_current_tok()
		return self.current_tok

	def update_current_tok(self):
		
		if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]

	def parse(self):
		res = self.statements()
		if not res.error and self.current_tok.type != TT_EOF:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Syntaxe Inválida"
			))
		return res

	###################################

	def statements(self):
		res = ParseResult()
		statements = []
		pos_start = self.current_tok.pos_start.copy()

		while self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

		statement = res.register(self.statement())
		if res.error: return res
		statements.append(statement)

		more_statements = True

		while True:
			newline_count = 0
			while self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()
				newline_count += 1
			if newline_count == 0:
				more_statements = False
			
			if not more_statements: break
			statement = res.try_register(self.statement())
			if not statement:
				self.reverse(res.to_reverse_count)
				more_statements = False
				continue
			statements.append(statement)

		return res.success(ListNode(
			statements,
			pos_start,
			self.current_tok.pos_end.copy()
		))
	
	def statement(self):
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()

		if self.current_tok.matches(TT_KEYWORD, 'retornar'):
			self.advance(res)

			expr = res.try_register(self.expr())
			if not expr:
				self.reverse(res.to_reverse_count)
			return res.success(ReturnNode(expr, pos_start, self.current_tok.pos_start.copy()))
	
		elif self.current_tok.matches(TT_KEYWORD, 'conferir'):
			self.advance(res)

			condition = res.register(self.expr())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'usando'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se 'usando'"
				))

			self.advance(res)

			error_to_raise = res.register(self.expr())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'detalhes'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se 'detalhes'"
				))

			self.advance(res)

			details = res.register(self.expr())
			if res.error: return res

			return res.success(CheckNode(condition, error_to_raise, details, pos_start, self.current_tok.pos_start.copy()))
		
		elif self.current_tok.matches(TT_KEYWORD, 'levantar'):
			self.advance(res)

			error_to_raise = res.register(self.expr())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'detalhes'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se 'detalhes'"
				))
			
			self.advance(res)

			details = res.register(self.expr())
			if res.error: return res

			return res.success(RaiseNode(error_to_raise, details, pos_start, self.current_tok.pos_start.copy()))
		
		elif self.current_tok.matches(TT_KEYWORD, 'importar'):
			self.advance(res)
			
			if not self.current_tok.type == TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se um identificador"
				))
			
			functions = []
			functions.append(self.current_tok)

			self.advance(res)
			
			while self.current_tok.type == TT_COMMA:
				self.advance(res)
				
				if not self.current_tok.type == TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se um identificador"
					))
				
				functions.append(self.current_tok)
				self.advance(res)

			if not self.current_tok.matches(TT_KEYWORD, 'de'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se 'de'"
				))
			
			self.advance(res)
			
			if not self.current_tok.type == TT_STRING:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se uma string"
				))
			
			module_path = self.current_tok

			self.advance(res)

			return res.success(ImportNode(module_path, functions, pos_start, self.current_tok.pos_start.copy()))

		elif self.current_tok.matches(TT_KEYWORD, 'continuar'):
			self.advance(res)
			return res.success(ContinueNode(pos_start, self.current_tok.pos_start.copy()))
		
		elif self.current_tok.matches(TT_KEYWORD, 'parar'):
			self.advance(res)
			return res.success(BreakNode(pos_start, self.current_tok.pos_start.copy()))
		
		expr = res.register(self.expr())
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Syntaxe Inválida"
			))
		return res.success(expr)

	def if_expr(self):
		res = ParseResult()
		all_cases = res.register(self.if_expr_cases('se'))
		if res.error: return res
		cases, else_case = all_cases
		return res.success(IfNode(cases, else_case))

	def if_expr_b(self):
		return self.if_expr_cases('mas')
	
	def if_expr_c(self):
		res = ParseResult()
		else_case = None

		if self.current_tok.matches(TT_KEYWORD, 'senão'):
			res.register_advancement()
			self.advance()

			if self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()

				statements = res.register(self.statements())
				if res.error: return res
				else_case = (statements, True)

				if self.current_tok.matches(TT_KEYWORD, 'fim'):
					res.register_advancement()
					self.advance()
				else:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se 'fim'"
					))
			else:
				expr = res.register(self.statement())
				if res.error: return res
				else_case = (expr, False)

		return res.success(else_case)

	def if_expr_b_or_c(self):
		res = ParseResult()
		cases, else_case = [], None

		if self.current_tok.matches(TT_KEYWORD, 'mas'):
			all_cases = res.register(self.if_expr_b())
			if res.error: return res
			cases, else_case = all_cases
		elif self.current_tok.matches(TT_KEYWORD, 'senão'):
			else_case = res.register(self.if_expr_c())
			if res.error: return res
			
		return res.success((cases, else_case))

	def if_expr_cases(self, case_keyword):
		res = ParseResult()
		cases = []
		else_case = None

		if not self.current_tok.matches(TT_KEYWORD, case_keyword):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se '{case_keyword}'"
			))

		res.register_advancement()
		self.advance()

		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'então'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'então'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			statements = res.register(self.statements())
			if res.error: return res
			cases.append((condition, statements, True))

			if self.current_tok.matches(TT_KEYWORD, 'fim'):
				res.register_advancement()
				self.advance()

		else:
			expr = res.register(self.statement())
			if res.error: return res
			cases.append((condition, expr, False))

		all_cases = res.register(self.if_expr_b_or_c())
		if res.error: return res
		new_cases, else_case = all_cases
		cases.extend(new_cases)

		return res.success((cases, else_case))

	def atom(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_INT, TT_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))
		
		elif tok.type == TT_STRING:
			res.register_advancement()
			self.advance()
			return res.success(StringNode(tok))

		elif tok.type == TT_IDENTIFIER:
			res.register_advancement()
			self.advance()

			if self.current_tok.type == TT_EQ or self.current_tok.type == TT_ARROW:
				res.register_advancement()
				self.advance()

				value = res.register(self.expr())
				if res.error: return res

				return res.success(VarAssignNode(tok, value))
			
			return res.success(VarAccessNode(tok))
		
		elif tok.matches(TT_KEYWORD, 'se'):
			if_expr = res.register(self.if_expr())
			if res.error: return res
			return res.success(if_expr)

		elif tok.matches(TT_KEYWORD, 'enquanto'):
			while_expr = res.register(self.while_expr())
			if res.error: return res
			return res.success(while_expr)
	
		elif tok.matches(TT_KEYWORD, 'função'):
			func_def = res.register(self.func_def())
			if res.error: return res
			return res.success(func_def)

		elif tok.matches(TT_KEYWORD, 'caminhando'):
			for_expr = res.register(self.for_expr())		
			if res.error: return res
			return res.success(for_expr)

		elif tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ')'"
				))
		
		elif tok.type == TT_LBRACKET:
			list_expr = res.register(self.list_expr())
			if res.error: return res
			return res.success(list_expr)

		return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Syntaxe Inválida"
		))
	
	def list_expr(self):
		res = ParseResult()
		element_nodes = []
		pos_start = self.current_tok.pos_start.copy()

		if self.current_tok.type != TT_LBRACKET:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se '['"
			))
		
		self.advance(res)

		if self.current_tok.type == TT_RBRACKET:
			self.advance(res)
			return res.success(ListNode(element_nodes, pos_start, self.current_tok.pos_end.copy()))
		else:
			element_nodes.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Syntaxe Inválida"
				))

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				element_nodes.append(res.register(self.expr()))
				if res.error: return res

			if self.current_tok.type != TT_RBRACKET:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ']' ou ','"
				))

			res.register_advancement()
			self.advance()
			return res.success(ListNode(element_nodes, pos_start, self.current_tok.pos_end.copy()))



	def func_def(self):
		res = ParseResult()
		if not self.current_tok.matches(TT_KEYWORD, 'função'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				'Esperava-se "função"'
			))
		
		self.advance(res)

		if self.current_tok.type == TT_IDENTIFIER:
			var_name_tok = self.current_tok
			self.advance(res)
			if not self.current_tok.matches(TT_KEYWORD, 'usando'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se 'usando'"
				))
			self.advance(res)
			if self.current_tok.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se '('"
				))
		else:
			var_name_tok = None
			if self.current_tok.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se '('"
				))
		
		self.advance(res)
		arg_name_toks = []

		if self.current_tok.type == TT_IDENTIFIER:
			arg_name_toks.append(self.current_tok)
			self.advance(res)

			while self.current_tok.type == TT_COMMA:
				self.advance(res)
				if not self.current_tok.type == TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						'Esperava-se um identificador'
					))
				
				arg_name_toks.append(self.current_tok)
				self.advance(res)

			if self.current_tok.type != TT_RPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ',' ou ')'"
				))
		
		else:
			if self.current_tok.type != TT_RPAREN:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ')'"
				))
		
		self.advance(res)
		if self.current_tok.type == TT_ARROW:
			self.advance(res)
			node_to_return = res.register(self.expr())
			if res.error: return res

			return res.success(FuncDefNode(var_name_tok, arg_name_toks, node_to_return, True))
		
		elif self.current_tok.type != TT_NEWLINE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '->' or NEWLINE"
			))

		res.register_advancement()
		self.advance()

		body = res.register(self.statements())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'fim'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'fim'"
			))

		res.register_advancement()
		self.advance()
		
		return res.success(FuncDefNode(
		var_name_tok,
		arg_name_toks,
		body,
		False
		))

	def for_expr(self):
		res = ParseResult()
		
		if not self.current_tok.matches(TT_KEYWORD, 'caminhando'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'caminhando'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se um identificador para a variável do loop"
			))

		var_name_tok = self.current_tok
		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_EQ:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se '=' após o identificador"
			))

		res.register_advancement()
		self.advance()

		start_value = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'até'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se 'até'"
			))

		res.register_advancement()
		self.advance()

		end_value = res.register(self.expr())
		if res.error: return res

		step_value = None
		if self.current_tok.matches(TT_KEYWORD, 'mudando'):
			res.register_advancement()
			self.advance()
			step_value = res.register(self.expr())
			if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'faça'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se 'faça'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			body = res.register(self.statements())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'fim'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se 'fim' para fechar o bloco 'caminhando'"
				))

			res.register_advancement()
			self.advance()
			return res.success(ForNode(var_name_tok, start_value, end_value, step_value, body, True))
		else:
			body = res.register(self.statement())
			if res.error: return res
			return res.success(ForNode(var_name_tok, start_value, end_value, step_value, body, False))
	
	def while_expr(self):
		res = ParseResult()
		if not self.current_tok.matches(TT_KEYWORD, 'enquanto'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start.copy(), self.current_tok.pos_end.copy(),
				'Esperava-se "enquanto"'
			))

		self.advance(res)
		condition = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'faça'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start.copy(), self.current_tok.pos_end.copy(),
				'Esperava-se "faça"'
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			body = res.register(self.statements())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'fim'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.pos_end,
					"Esperava-se 'fim'"
				))

			res.register_advancement()
			self.advance()

			return res.success(WhileNode(condition, body, True))
		else:
			body = res.register(self.statement())
			if res.error: return res
			return res.success(WhileNode(condition, body, False))


	def power(self):
		return self.bin_op(self.call, (TT_POW, ), self.factor)
	
	def call(self):
		res = ParseResult()
		atom = res.register(self.atom())
		if res.error: return res

		if self.current_tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			arg_nodes = []

			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
			else:
				arg_nodes.append(res.register(self.expr()))
				if res.error:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Syntaxe Inválida"
					))

				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()

					arg_nodes.append(res.register(self.expr()))
					if res.error: return res

				if self.current_tok.type != TT_RPAREN:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se ')' ou ','"
					))

				res.register_advancement()
				self.advance()
			return res.success(CallNode(atom, arg_nodes))
		return res.success(atom)

	def factor(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_PLUS, TT_MINUS):
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))

		return self.power()

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV))

	def comp_expr(self):
		res = ParseResult()

		if self.current_tok.matches(TT_KEYWORD, 'não'):
			op_tok = self.current_tok
			self.advance(res)

			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode(op_tok, node))
		
		node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_GT, TT_GTE, TT_LT, TT_LTE, (TT_KEYWORD, 'igual'))))
		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Syntaxe Inválida, esperava-se 'não', '==', '!=', '<', '<=', '>', '>='..."
			))

		return res.success(node)
	
	def arith_expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

	def expr(self):
		res = ParseResult()
		constant = False

		if self.current_tok.matches(TT_KEYWORD, 'declarar'):
			res.register_advancement()
			self.advance()

			if self.current_tok.matches(TT_KEYWORD, 'constante'):
				res.register_advancement()
				self.advance()
				constant = True

			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se um identificador"
				))

			var_name = self.current_tok
			res.register_advancement()
			self.advance()

			if self.current_tok.type == TT_SEMICOLON:
				return res.success(VarAssignNode(var_name, None, constant=constant))
			elif not (self.current_tok.matches(TT_KEYWORD, 'como') or self.current_tok.type in (TT_EQ, TT_ARROW)):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se '=' ou '->' ou ';'",
				))

			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			return res.success(VarAssignNode(var_name, expr, constant=constant))

		node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'e'), (TT_KEYWORD, 'ou'))))

		if res.error:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				'Syntaxe Inválida'
			))

		return res.success(node)

	###################################

	def bin_op(self, func_a, ops, func_b=None):
		if func_b == None:
			func_b = func_a
		
		res = ParseResult()
		if res.error: return res

		left = res.register(func_a())
		if res.error: return res

		while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
				
			left = BinOpNode(left, op_tok, right)

		return res.success(left)

#######################################
# RUNTIME RESULT
#######################################

class RTResult:
	def __init__(self):
		self.reset()
	def reset(self):
		self.value = None
		self.error = None
		self.func_return_value = None
		self.loop_should_continue = False
		self.loop_should_break = False

	def register(self, res):
		self.func_return_value = res.func_return_value
		self.loop_should_break = res.loop_should_break
		self.loop_should_continue = res.loop_should_continue
		
		if res.error: self.error = res.error
		return res.value

	def success(self, value):
		self.reset()
		self.value = value
		return self

	def success_return(self, value):
		self.reset()
		self.func_return_value = value
		return self
	
	def success_continue(self):
		self.reset()
		self.loop_should_continue = True
		return self
	
	def success_break(self):
		self.reset()
		self.loop_should_break = True
		return self
	
	def should_return(self):
		return (
			self.error or
			self.func_return_value or
			self.loop_should_break or
			self.loop_should_continue
		)

	def failure(self, error):
		self.reset()
		self.error = error
		return self

#######################################
# VALUES
#######################################

class Value:
	def __init__(self):
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		return None, self.illegal_operation(other)

	def subbed_by(self, other):
		return None, self.illegal_operation(other)

	def multed_by(self, other):
		return None, self.illegal_operation(other)

	def dived_by(self, other):
		return None, self.illegal_operation(other)

	def powed_by(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)

	def anded_by(self, other):
		return None, self.illegal_operation(other)

	def ored_by(self, other):
		return None, self.illegal_operation(other)

	def notted(self):
		
		return None, self.illegal_operation()

	def execute(self, args):
		return RTResult().failure(self.illegal_operation())

	def copy(self):
		raise Exception('Nenhum copy() metódo encontrado')

	def is_true(self):
		return False

	def illegal_operation(self, other=None):
		if not other: other = self
		return RTError(
			self.pos_start, other.pos_end,
			'Operação Ilegal',
			self.context
		)

class Number(Value):

	def __init__(self, value):
		self.value = value
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None

	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RTError(
					other.pos_start, other.pos_end,
					'Division by zero',
					self.context
				)

			return Number(self.value / other.value).set_context(self.context), None

	def powed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value ** other.value).set_context(self.context), None
	
	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return Number(0).set_context(self.context), None
	
	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(int(self.value != other.value)).set_context(self.context), None
		else:
			return Number(1).set_context(self.context), None
	
	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value < other.value)).set_context(self.context), None
	
	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value <= other.value)).set_context(self.context), None
	
	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value > other.value)).set_context(self.context), None
	
	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value >= other.value)).set_context(self.context), None
	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.is_true() and other.is_true())).set_context(self.context), None
	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.is_true() or other.is_true())).set_context(self.context), None
	def notted(self):
		return Number(int(0 if self.is_true() else 1)).set_context(self.context), None

	def is_true(self):
		return int(True if self.value > 0 else False)
	
	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	
	def __repr__(self):
		return str(self.value)


Number.true = Number(1)
Number.false = Number(0)
Number.null = Number(0)

class String(Value):
	def __init__(self, value):
		self.value = value
		self.set_pos()
		self.set_context()
	
	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value + other.value).set_context(self.context), None
		elif isinstance(other, List):
			return String(self.value + ", ".join(str(x) for x in other.elements)).set_context(self.context), None
		elif isinstance(other, Number):
			return String(self.value + str(other.value)).set_context(self.context), None
		else:
			return String(self.value + repr(other)).set_context(self.context), None

	def multed_by(self, other):
		if isinstance(other, Number):
			return String(self.value * int(other.value)).set_context(self.context), None
		return None, self.illegal_operation(other)
	
	def notted(self):
		return Number(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def anded_by(self, other):
		return Number(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def ored_by(self, other):
		return Number(int(self.is_true() or other.is_true())).set_context(self.context), None

	def get_comparison_eq(self, other):
		if isinstance(other, String):
			return Number(int(self.value == other.value)).set_context(self.context), None
		return Number.false, None
	
	def get_comparison_ne(self, other):
		if isinstance(other, String):
			return Number(int(self.value != other.value)).set_context(self.context), None
		return Number.true, None
		
	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def is_true(self):
		return len(self.value) > 0 and self.value
	
	def __str__(self):
		return self.value
	
	def __repr__(self):
		return f'"{self.value}"'

class List(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements
	
	def added_to(self, other):
		new_list = self.copy()
		new_list.elements.append(other)
		return new_list, None
	
	def multed_by(self, other):
		if isinstance(other, List):
			new_list = self.copy()
			new_list.elements.extend(other.elements)
			return new_list, None
		return None, self.illegal_operation(other)
	
	def get_comparison_eq(self, other):
		if isinstance(other, List):
			return Number(int(self.elements == other.elements)).set_context(self.context), None
		return Number.false, None
	
	def get_comparison_ne(self, other):
		if isinstance(other, List):
			return Number(int(self.elements != other.elements)).set_context(self.context), None
		return Number.true, None
	
	def notted(self):
		return Number(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def anded_by(self, other):
		return Number(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def _ored_by(self, other):
		return Number(int(self.is_true() or other.is_true())).set_context(self.context), None
	
	def is_true(self):
		return len(self.elements) > 0

	def copy(self):
		copy = List(self.elements)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
	def __str__(self):
		return f'{", ".join([str(x) for x in self.elements])}'
	
	def __repr__(self):
		return f'[{", ".join([str(x) for x in self.elements])}]'

class BaseFunction(Value):
	def __init__(self, name):
		super().__init__()
		self.name = name or "<anonymous>"
	
	def generate_new_context(self):
		new_context = Context(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
		return new_context

	def check_args(self, arg_names, args):
		res = RTResult()

		if len(args) > len(arg_names):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"{len(args) - len(arg_names)} a mais de argumentos foram passados a função '{self.name}'",
				self.context
			))
		
		if len(args) < len(arg_names):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"{len(arg_names) - len(args)} a menos de argumentos foram passados a função '{self.name}'",
				self.context
			))
		
		return res.success(None)
	
	def populate_args(self, arg_names, args, exec_ctx):
		for i in range(len(args)):
			arg_name = arg_names[i]
			arg_value = args[i]
			arg_value.set_context(exec_ctx)
			exec_ctx.symbol_table.set(arg_name, arg_value)
	
	def check_and_populate_args(self, arg_names, args, exec_ctx):
		res = RTResult()
		res.register(self.check_args(arg_names, args))
		if res.should_return(): return res
		self.populate_args(arg_names, args, exec_ctx)
		return res.success(None)
	
	def __repr__(self):
		return f"<função {self.name} em {FILENAME}>"


class Function(BaseFunction):
	def __init__(self, name, body_node, arg_names, should_auto_return=True):
		super().__init__(name)
		self.body_node = body_node
		self.arg_names = arg_names
		self.should_auto_return = should_auto_return

	def execute(self, args):
		res = RTResult()
		interpreter = Interpreter()
		new_context = self.generate_new_context()

		res.register(self.check_and_populate_args(self.arg_names, args, new_context))
		if res.should_return(): return res

		value = res.register(interpreter.visit(self.body_node, new_context))
		if res.should_return() and res.func_return_value == None: return res

		ret_value = (value if self.should_auto_return else None) or res.func_return_value or Number.null
		return res.success(ret_value)

	def copy(self):
		copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

class BuiltInFunction(BaseFunction):
	def __init__(self, name):
		super().__init__(name)
	
	def execute(self, args):
		res = RTResult()
		exec_ctx = self.generate_new_context()

		method_name = f'execute_{self.name}'
		method = getattr(self, method_name, self.no_visit_method)

		res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
		if res.should_return(): return res

		return_value = res.register(method(exec_ctx))
		if res.should_return(): return res
		return res.success(return_value)

	def no_visit_method(self, node, context):
		raise Exception(f'Nenhum execute_{self.name} metódo encontrado')

	def copy(self):
		copy = BuiltInFunction(self.name)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy

	def __repr__(self):
		return f"< função predefinida {self.name} em (<stdlib>) >"
	
	def execute_falar(self, exec_ctx):
		print(str(exec_ctx.symbol_table.get('value')))
		return RTResult().success(str(exec_ctx.symbol_table.get('value')))
	execute_falar.arg_names = ['value']

	def execute_pegar(self, exec_ctx):
		lst = exec_ctx.symbol_table.get('list')
		if not isinstance(lst, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{lst}' não é uma lista.",
				exec_ctx
			))
		index = exec_ctx.symbol_table.get('index')
		if not isinstance(index, Number):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{index}' não é um número.",
				exec_ctx
			))
		try:
			return RTResult().success(lst.elements[int(index.value)])
		except:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Indice '{index}' fora do intervalo, index não existe.",
				exec_ctx
			))
	execute_pegar.arg_names = ['list', 'index']
	
	def execute_len(self, exec_ctx):
		val = exec_ctx.symbol_table.get('value')
		if isinstance(val, List):
			return RTResult().success(Number(len(val.elements)))
		elif isinstance(val, String):
			return RTResult().success(Number(len(val.value)))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{val}' não é uma lista ou uma string.",
				exec_ctx
			))
	execute_len.arg_names = ['value']

	def execute_procurar(self, exec_ctx):
		lst = exec_ctx.symbol_table.get('list')
		val = exec_ctx.symbol_table.get('value')

		for i in lst.elements:
			if val.value == i.value:
				return RTResult().success(Number(lst.elements.index(i)))

		return RTResult().success(Number(-1))
	execute_procurar.arg_names = ['list', 'value']

	def execute_colocar(self, exec_ctx):
		lst = exec_ctx.symbol_table.get('list')
		val = exec_ctx.symbol_table.get('value')

		lst.elements.append(val)
		return RTResult().success(val)
	execute_colocar.arg_names = ['list', 'value']

	def execute_estourar(self, exec_ctx):
		lst = exec_ctx.symbol_table.get('list')
		index = exec_ctx.symbol_table.get('index')

		try:
			return RTResult().success(lst.elements.pop(int(index.value)))
		except:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Indice '{index}' fora do intervalo, index não existe.",
				exec_ctx
			))
	execute_estourar.arg_names = ['list', 'index']

	def execute_perguntar(self, exec_ctx):
		text = input(exec_ctx.symbol_table.get('prompt').value)
		return RTResult().success(String(text))
	execute_perguntar.arg_names = ['prompt']

	def execute_input_int(self, exec_ctx):
		while True:
			text = input(exec_ctx.symbol_table.get('prompt').value)
			try:
				number = int(text)
				return RTResult().success(Number(number))
			except ValueError:
				print(f"'{text}' nao é um número.")
				continue
	execute_input_int.arg_names = ['prompt']
	
	def execute_número(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number):
			return RTResult().success(Number(exec_ctx.symbol_table.get('value').value))
		else:
			return RTResult().success(Number.false)
	execute_número.arg_names = ['value']
	
	def execute_string(self, exec_ctx):
		return RTResult().success(String(exec_ctx.symbol_table.get('value').value))
	execute_string.arg_names = ['value']
	
	def execute_frase(self, exec_ctx):
		return RTResult().success(Number(int(isinstance(exec_ctx.symbol_table.get('value'), String))))
	execute_frase.arg_names = ['value']
	
	def execute_lista(self, exec_ctx):
		return RTResult().success(Number(int(isinstance(exec_ctx.symbol_table.get('value'), List))))
	execute_lista.arg_names = ['value']
	
	def execute_func(self, exec_ctx):
		return RTResult().success(Number(int(isinstance(exec_ctx.symbol_table.get('value'), Function))))
	execute_func.arg_names = ['value']

	def execute_picotar(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), String):
			return RTResult().success(List(exec_ctx.symbol_table.get('value').value.split()))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{exec_ctx.symbol_table.get('value')}' não é uma string.",
				exec_ctx
			))
	execute_picotar.arg_names = ['value']

	def execute_limpar(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), String):
			return RTResult().success(List(exec_ctx.symbol_table.get('value').value.strip()))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{exec_ctx.symbol_table.get('value')}' não é uma string.",
				exec_ctx
			))
	execute_limpar.arg_names = ['value']

	def execute_extender(self, exec_ctx):
		listA = exec_ctx.symbol_table.get("listA")
		listB = exec_ctx.symbol_table.get("listB")

		if not isinstance(listA, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Primeiro argumento deve ser uma lista",
				exec_ctx
			))

		if not isinstance(listB, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Segundo argumento deve ser uma lista",
				exec_ctx
			))

		listA.elements.extend(listB.elements)
		return RTResult().success(List(listA.elements).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
	execute_extender.arg_names = ["listA", "listB"]

	def execute_quadrado(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number):
			return RTResult().success(Number(exec_ctx.symbol_table.get('value').value ** 2))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{exec_ctx.symbol_table.get('value')}' não é um número.",
				exec_ctx
			))
	execute_quadrado.arg_names = ['value']

	def execute_cubo(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number):
			return RTResult().success(Number(exec_ctx.symbol_table.get('value').value ** 3))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{exec_ctx.symbol_table.get('value')}' não é um número.",
				exec_ctx
			))
	execute_cubo.arg_names = ['value']

	def execute_raiz(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number):
			return RTResult().success(Number(exec_ctx.symbol_table.get('value').value ** (1/2)))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{exec_ctx.symbol_table.get('value')}' não é um número.",
				exec_ctx
			))
	execute_raiz.arg_names = ['value']

#######################################
# CONTEXT
#######################################

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None

#######################################
# SYMBOL TABLE
#######################################

class SymbolTable:
	def __init__(self, parent=None):
		self.symbols = {}
		self.constants = []
		self.parent = parent

	def get(self, name):
		value = self.symbols.get(name, None)
		if value == None and self.parent:
			return self.parent.get(name)
		return value

	def set(self, name, value, constant=False):
		if name in self.constants:
			raise Exception(f"'{name}' é uma constante e não pode ser alterada")
		self.symbols[name] = value
		if constant: self.constants.append(name)

	def remove(self, name):
		del self.symbols[name]

#######################################
# INTERPRETER
#######################################

class Interpreter:
	def visit(self, node, context):
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context):
		raise Exception(f'Nenhum visit_{type(node).__name__} metódo encontrado')

	###################################

	def visit_NumberNode(self, node, context):
		return RTResult().success(
			Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)
	
	def visit_StringNode(self, node, context):
		return RTResult().success(
			String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)

	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
		value = context.symbol_table.get(var_name)

		if not value:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"'{var_name}' nunca foi declarado",
				context
			))

		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(value)

	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
		if node.value_node: value = res.register(self.visit(node.value_node, context))
		else: value = Number(0).set_context(context).set_pos(node.pos_start, node.pos_end)
		if res.should_return(): return res

		context.symbol_table.set(var_name, value, node.constant)
		return res.success(value)
	
	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []

		for element_node in node.element_nodes:
			elements.append(res.register(self.visit(element_node, context)))
			if res.should_return(): return res

		return res.success(List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_CheckNode(self, node, context):
		res = RTResult()

		condition = res.register(self.visit(node.condition_node, context))
		if res.should_return(): return res

		if condition.is_true():
			return res.success(Number(1).set_context(context).set_pos(node.pos_start, node.pos_end))
		else:
			error_name = res.register(self.visit(node.error_name, context))
			details = res.register(self.visit(node.details, context))

			return res.failure(Error(
				node.pos_start, node.pos_end,
				repr(error_name), repr(details)
			))
	
	def visit_RaiseNode(self, node, context):
		res = RTResult()

		error_name = res.register(self.visit(node.error_name, context))
		details = res.register(self.visit(node.details, context))

		return res.failure(Error(
			node.pos_start, node.pos_end,
			repr(error_name), repr(details)
		))
	
	def visit_ImportNode(self, node, context):
		res = RTResult()
    
		file_path = node.file_path
		module_name = file_path.value
		
		current_dir = os.path.dirname(os.path.abspath(FILENAME))
		full_path = f"../{module_name}.neb" if module_name != 'stdlibs' else os.path.join(current_dir, 'modules/stdlibs.neb')
		
		if not os.path.exists(full_path):
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"Module '{module_name}' not found",
				context
			))
		
		try:
			with open(full_path, 'r', encoding='utf-8') as f:
				text = f.read()
		except Exception as e:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"Failed to read module '{module_name}': {str(e)}",
				context
			))
		
		result, error, module_ctx = run(full_path, text)
		if error:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"Error in module '{module_name}': {error.as_string()}",
				context
			))
		
		# Import the specified functions
		for func_tok in node.functions:
			func_name = func_tok.value
			func_value = module_ctx.symbol_table.get(func_name)
			if not func_value:
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Function '{func_name}' not found in module '{module_name}'",
					context
				))
			context.symbol_table.set(func_name, func_value)
		
		return res.success(Number.null)


	def visit_FuncDefNode(self, node, context):
		res = RTResult()

		func_name = node.var_name_tok.value if node.var_name_tok else None
		body_node = node.body_node
		arg_names = [arg_name.value for arg_name in node.arg_name_toks]
		func_value = Function(func_name, body_node, arg_names, node.should_auto_return).set_context(context).set_pos(node.pos_start, node.pos_end)

		if node.var_name_tok:
			context.symbol_table.set(func_name, func_value)

		return res.success(func_value)
	
	def visit_CallNode(self, node, context):
		res = RTResult()
		args = []

		value_to_call = res.register(self.visit(node.node_to_call, context))
		if res.should_return(): return res
		value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.should_return(): return res

		return_value = res.register(value_to_call.execute(args))
		if res.should_return(): return res

		return res.success(return_value)
	
	def visit_IfNode(self, node, context):
		res = RTResult()

		for condition, expr, should_return_null in node.cases:
			condition_value = res.register(self.visit(condition, context))
			if res.should_return(): return res

			if condition_value.is_true():
				expr_value = res.register(self.visit(expr, context))
				if res.should_return(): return res
				return res.success(Number.null if should_return_null else expr_value)

		if node.else_case:
			expr, should_return_null = node.else_case
			expr_value = res.register(self.visit(expr, context))
		if res.should_return(): return res
		return res.success(Number.null if should_return_null else expr_value)

	def visit_BinOpNode(self, node, context):
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.should_return(): return res
		right = res.register(self.visit(node.right_node, context))
		if res.should_return(): return res

		if node.op_tok.type == TT_PLUS:
			result, error = left.added_to(right)
		elif node.op_tok.type == TT_MINUS:
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TT_MUL:
			result, error = left.multed_by(right)
		elif node.op_tok.type == TT_DIV:
			result, error = left.dived_by(right)
		elif node.op_tok.type == TT_POW:
			result, error = left.powed_by(right)
		elif node.op_tok.type == TT_EE or node.op_tok.matches(TT_KEYWORD, 'igual'):
			result, error = left.get_comparison_eq(right)
		elif node.op_tok.type == TT_NE:
			result, error = left.get_comparison_ne(right)
		elif node.op_tok.type == TT_LT:
			result, error = left.get_comparison_lt(right)
		elif node.op_tok.type == TT_LTE:
			result, error = left.get_comparison_lte(right)
		elif node.op_tok.type == TT_GT:
			result, error = left.get_comparison_gt(right)
		elif node.op_tok.type == TT_GTE:
			result, error = left.get_comparison_gte(right)
		elif node.op_tok.matches(TT_KEYWORD, 'e'):
			result, error = left.anded_by(right)
		elif node.op_tok.matches(TT_KEYWORD, 'ou'):
			result, error = left.ored_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node, context):
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.should_return(): return res

		error = None

		if node.op_tok.type == TT_MINUS:
			number, error = number.multed_by(Number(-1))
		elif node.op_tok.matches(TT_KEYWORD, 'não'):
			number, error = number.notted()

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))
	
	def visit_ForNode(self, node, context):
		elements = []
		res = RTResult()
		var_name = node.var_name_tok.value
		start_value = res.register(self.visit(node.start_value_node, context))
		if res.should_return(): return res

		end_value = res.register(self.visit(node.end_value_node, context))
		if res.should_return(): return res

		if node.step_value_node:
			step_value = res.register(self.visit(node.step_value_node, context))
			if res.should_return(): return res
		else:
			step_value = Number(1).set_context(context).set_pos(node.pos_start, node.pos_end)

		i = start_value.value

		if step_value.value >= 0:
			condition = lambda i: i < end_value.value
		else:
			condition = lambda i: i > end_value.value

		while condition(i):
			context.symbol_table.set(var_name, Number(i))
			i += step_value.value

			value = (res.register(self.visit(node.body_node, context)))
			if res.should_return() and res.loop_should_break == False and res.loop_should_continue == False: return res

			if res.loop_should_continue:
				continue
			elif res.loop_should_break:
				break

			elements.append(value)

		return res.success(Number.null if node.should_return_null else List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_WhileNode(self, node, context):
		res = RTResult()
		elements = []

		while True:
			condition = res.register(self.visit(node.condition_node, context))
			if res.should_return(): return res

			if not condition.is_true(): break

			value = (res.register(self.visit(node.body_node, context)))
			if res.should_return() and res.loop_should_break == False and res.loop_should_continue == False: return res

			if res.loop_should_break:
				break
			elif res.loop_should_continue:
				continue

			elements.append(value)

		return res.success(Number.null if node.should_return_null else List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_ReturnNode(self, node, context):
		res = RTResult()

		if node.node_to_return:
			value = res.register(self.visit(node.node_to_return, context))
			if res.should_return(): return res
		else:
			value = Number.null

		return res.success_return(value)
	
	def visit_ContinueNode(self, node, context):
		return RTResult().success_continue()
	
	def visit_BreakNode(self, node, context):
		return RTResult().success_break()

#######################################
# RUN
#######################################

global_symbol_table = SymbolTable()
global_symbol_table.set("nulo", Number.null, True)
global_symbol_table.set("verdadeiro", Number.true, True)
global_symbol_table.set("falso", Number.false, True)
global_symbol_table.set("falar", BuiltInFunction("falar"), True)
global_symbol_table.set("pegar", BuiltInFunction("pegar"), True)
global_symbol_table.set("perguntar", BuiltInFunction("perguntar"), True)
global_symbol_table.set("input_int", BuiltInFunction("input_int"), True)
global_symbol_table.set("extender", BuiltInFunction("extender"), True)
global_symbol_table.set("número", BuiltInFunction("número"), True)
global_symbol_table.set("raiz", BuiltInFunction("raiz"), True)
global_symbol_table.set("cubo", BuiltInFunction("cubo"), True)
global_symbol_table.set("quadrado", BuiltInFunction("quadrado"), True)
global_symbol_table.set("string", BuiltInFunction("string"), True)
global_symbol_table.set("len", BuiltInFunction("len"), True)
global_symbol_table.set("frase", BuiltInFunction("frase"), True)
global_symbol_table.set("picotar", BuiltInFunction("picotar"), True)
global_symbol_table.set("lista", BuiltInFunction("lista"), True)
global_symbol_table.set("func", BuiltInFunction("func"), True)
global_symbol_table.set("procurar", BuiltInFunction("procurar"), True)
global_symbol_table.set("colocar", BuiltInFunction("colocar"), True)
global_symbol_table.set("estourar", BuiltInFunction("estourar"), True)
global_symbol_table.set("limpar", BuiltInFunction("limpar"), True)

def run(fn, text):
	global FILENAME
	# Generate tokens
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error, None
	
	FILENAME = lexer.fn

	# Generate AST
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error, None

	# Run program
	interpreter = Interpreter()
	context = Context('<program>')
	context.symbol_table = SymbolTable(global_symbol_table)
	result = interpreter.visit(ast.node, context)
	return result.value, result.error, context