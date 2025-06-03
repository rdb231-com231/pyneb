#######################################
# IMPORTS
#######################################

from string_with_arrows import *

import sys
import string
import unicodedata
import os
import random
import subprocess
from typing import Literal
import time

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS += 'áéíóúãâêôãõçÁÉÍÓÚÃÂÊÔÃÕÇ_'	
LETTERS_DIGITS = LETTERS + DIGITS
FILENAME = ''
ALL_MODULES = [item for item in os.listdir('modules') if item.endswith('.neb')]

DEFAULT_CONFIG = {
	'version': "1.5.0",
	'author': 'mewplush',
	'publisher': 'mewplush',
	'interpreter': 'pyneb',
	'python': {
		'author-version':'3.12.10',
		'user-version': sys.version,
	},
	'description': 'PyNeb é o interpretador principal da linguagem Nebula. Escrito em Python, ele é capaz de executar scripts Nebula e também pode ser usado como um shell interativo.',
	'special-keys': {
		'__shell__': 'Nebula Shell',
		'.config': 'Configuração do PyNeb',
		'__path__': 'Caminho do arquivo',
		'.version': 'Versão do PyNeb',
		'setmain <filename>': 'Define o arquivo principal',
		'.': 'Executar arquivo principal'
	}
}
DEFAULT_CONFIG['special-keys'].update({key: value for key, value in DEFAULT_CONFIG.items()})

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
		result += f'Arquivo {self.pos_start.fn}, linha {self.pos_start.ln + 1}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt if self.pos_start else None, self.pos_start, self.pos_end)
		return result

class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Caractere Ilegal', details)

class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Syntaxe Inválida', details)

class CheckError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Erro Conferindo Informações', details)

class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context, name=None):
		super().__init__(pos_start, pos_end, name or 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt if self.pos_start else None, self.pos_start, self.pos_end)
		return result + "\n"

	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.context

		while ctx and pos:
			result = f'  Arquivo {pos.fn}, linha {str(pos.ln + 1)}, em {ctx.display_name}\n' + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent

		return '\nErro Encontrado (Chamada mais recente):\n' + result + ""

class ValorError(RTError):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, details, context, 'Erro de Valor')



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
TT_COLON = 'COLON'
TT_LBRACE = 'LBRACE'
TT_RBRACE = 'RBRACE'
TT_DOT = 'DOT'
TT_FTEXT = 'FTEXT'
TT_FEXPR = 'FEXPR'
TT_PE = 'PE'
TT_ME = 'ME'

KEYWORDS = [
	'declarar',
	'constante',
	'assim',
	'também',
	'como',
	'ou',
	'e',
	'função',
	'não',
	'se',
	'então',
	'senão',
	'classe',
	'mas',
	'enquanto',
	'caminhando',
	'até',
	'para',
	'cada',
	'mudando',
	'faça',
	'igual',
	'usando',
	'fim',
	'retornar',
	'continuar',
	'parar',
	'conferir',
	'levantar',
	'detalhes',
	'importar',
	'de',
	'verdadeiro',
	'falso',
	'em',
	'privada',
	'lista',
	'tupla',
	'mesa',
	'dicionário',
	'string',
	'número',
	'extende',
	'super',
	'tentar',
	'comparar',
	'caso'
]

class Token:
	def __init__(self, type_, value=None, pos_start=None, pos_end=None, parts=None):
		
		self.type = type_
		self.value = value
		self.parts = parts

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
		sys.setrecursionlimit(3000)
		self.fn = fn
		self.text = text
		self.pos = Position(-1, 0, -1, fn, text)
		self.current_char = None
		self.advance()
	
	def advance(self):
		self.pos.advance(self.current_char)
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
	
	def peek(self):
		peek_pos = self.pos.idx + 1
		return self.text[peek_pos] if peek_pos < len(self.text) else None


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
				pos_start = self.pos.copy()
				self.advance()
				if self.current_char == '=':
					tokens.append(Token(TT_PE, pos_start=pos_start, pos_end=self.pos))
					self.advance()
				else:
					tokens.append(Token(TT_PLUS, pos_start=pos_start, pos_end=self.pos))
			elif self.current_char == '-':
				pos_start = self.pos.copy()
				self.advance()
				if self.current_char == '=':
					tokens.append(Token(TT_ME, pos_start=pos_start, pos_end=self.pos))
					self.advance()
				else:
					tokens.append(Token(TT_MINUS, pos_start=pos_start, pos_end=self.pos))
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
			elif self.current_char == '.':
				tokens.append(Token(TT_DOT, pos_start=self.pos))
				self.advance()
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
			elif self.current_char == '{':
				tokens.append(Token(TT_LBRACE, pos_start=self.pos))
				self.advance()
			elif self.current_char == '}':
				tokens.append(Token(TT_RBRACE, pos_start=self.pos))
				self.advance()
			elif self.current_char == ':':
				tokens.append(Token(TT_COLON, pos_start=self.pos))
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
			return None, IllegalCharError(pos_start, self.pos, "Esperava-se '=' depois de '!'")
	
	def skip_comment(self):
		self.advance()

		while self.current_char != '\n' and self.current_char:
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
		string_parts = []
		full_string = ""
		pos_start = self.pos.copy()
		self.advance()
		current_text = ""
		escape_char = False
		escape_characters = {
			'n': '\n', 't': '\t', 'r': '\r', 'b': '\b', 'f': '\f',
			'"': '"', '\\': '\\', '$': '$'
		}
		
		while self.current_char is not None and (self.current_char != '"' or escape_char):
			full_string += self.current_char
			if escape_char:
				current_text += escape_characters.get(self.current_char, self.current_char)
				escape_char = False
				self.advance()
			elif self.current_char == "\\":
				escape_char = True
				self.advance()
			elif self.current_char == "$" and self.peek() == "{":
				if current_text:
					string_parts.append(('TEXT', current_text))
					current_text = ""
				
				self.advance()
				self.advance()
				
				expr_tokens = []
				while self.current_char is not None and self.current_char != "}":
					full_string += self.current_char
					expr_tokens.append(self.current_char)
					self.advance()
				
				if self.current_char != "}":
					return None, IllegalCharError(pos_start, self.pos, "Expressão não fechada com }")
				
				self.advance()
				
				expr_lexer = Lexer(f"<fstring>", "".join(expr_tokens))
				tokens, error = expr_lexer.make_tokens()
				if error: return None, error
				
				string_parts.append(('EXPR', tokens))
			else:
				current_text += self.current_char
				self.advance()
		
		if current_text:
			string_parts.append(('TEXT', current_text))
		
		if self.current_char != '"':
			return None, IllegalCharError(pos_start, self.pos, "String não fechada")
		
		self.advance()
		return Token(TT_STRING, ''.join(full_string), pos_start, self.pos, parts=(string_parts, full_string))
#######################################
# NODES
#######################################

class FuncDefNode:
	def __init__(self, var_name_tok, arg_name_toks, body_node, should_auto_return, private=False, default_vals=None):
		self.var_name_tok = var_name_tok
		self.arg_name_toks = arg_name_toks
		self.body_node = body_node
		self.should_auto_return = should_auto_return
		self.private = private
		self.default_vals = default_vals

		if self.var_name_tok:
			self.pos_start = self.var_name_tok.pos_start
		elif len(self.arg_name_toks) > 0:
			self.pos_start = self.arg_name_toks[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start
		
		self.pos_end = self.body_node.pos_end
	
	def __repr__(self):
		return f'FuncDefNode({self.var_name_tok}, {self.arg_name_toks}, {self.body_node}, {self.should_auto_return})'

class SuperNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end

	def __repr__(self):
		return "super()"

class ClassNode:
	def __init__(self, class_name_tok, body_node, inheritance):
		self.class_name_tok = class_name_tok
		self.body_node = body_node
		self.inheritance = inheritance
		self.pos_start = self.class_name_tok.pos_start
		self.pos_end = self.body_node.pos_end if body_node else class_name_tok.pos_end
	
	def __repr__(self):
		return f'ClassNode({self.class_name_tok}, {self.body_node}, {self.inheritance})'

class SwitchNode:
	def __init__(self, expr_to_compare, cases, default_case, pos_start, pos_end):
		self.expr_to_compare = expr_to_compare
		self.cases = cases # [(val, body, is_block, pos_start)...]
		self.default_case = default_case # (body, is_block, pos_start) or None

		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f'SwitchNode({self.expr_to_compare}, {self.cases}, {self.default_case})'

class CallNode:
	def __init__(self, node_to_call, arg_nodes):
		self.node_to_call = node_to_call
		self.arg_nodes = arg_nodes

		self.pos_start = self.node_to_call.pos_start

		if len(self.arg_nodes) > 0:
			self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
		else:
			self.pos_end = self.node_to_call.pos_end
	
	def __repr__(self):
		return f'{repr(self.node_to_call)}({[repr(arg) for arg in self.arg_nodes]})'

class NumberNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		
		return f'{self.tok}'

class BoolNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end
	
	def __repr__(self):
		return f'\'{self.tok}\''

class StringNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = tok.pos_start
		self.pos_end = tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

class FStringNode:
	def __init__(self, parts, pos_start, pos_end):
		self.parts = parts

		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f"f'{self.parts}'"
	

class ListNode:
	def __init__(self, element_nodes, pos_start=None, pos_end=None):
		self.element_nodes = element_nodes

		self.pos_start = pos_start if pos_start else self.element_nodes.pos_start
		self.pos_end = pos_end if pos_end else self.element_nodes.pos_end

	def __repr__(self):
			return f'[{", ".join([repr(x) for x in self.element_nodes])}]'

class TupleNode:
	def __init__(self, element_nodes, pos_start=None, pos_end=None):
		self.element_nodes = element_nodes
		self.pos_start = pos_start
		self.pos_end = pos_end

	def __repr__(self):
		return f'({", ".join([repr(x) for x in self.element_nodes])})'

class DictNode:
	def __init__(self, pairs, pos_start=None, pos_end=None):
		self.pairs = pairs

		self.pos_start = pos_start if pos_start else self.pairs[0].pos_start
		self.pos_end = pos_end if pos_end else self.pairs[len(self.pairs) - 1].pos_end

	def __repr__(self):
		return f'{{{", ".join([f"{str(k.value)}: {repr(v.copy())}" for k, v in self.pairs])}}}'

class SetNode:
	def __init__(self, elements, pos_start=None, pos_end=None):
		self.elements = elements

		self.pos_start = pos_start if pos_start else self.elements[0].pos_start
		self.pos_end = pos_end if pos_end else self.elements[-1].pos_end
	
	def __repr__(self):
		self.reprs = [", ".join(self.elements)]
		return "{" + self.reprs + "}"

class AttrAccessNode:
	def __init__(self, obj_node, attr_name_tok):
		self.obj_node = obj_node
		self.attr_name_tok = attr_name_tok
		self.pos_start = obj_node.pos_start
		self.pos_end = attr_name_tok.pos_end
	
	def __repr__(self):
		return f'{self.obj_node}.{self.attr_name_tok}'

class AttrAssignNode:
	def __init__(self, obj_node, attr_name_tok, value_node):
		self.obj_node = obj_node
		self.attr_name_tok = attr_name_tok
		self.value_node = value_node

		self.pos_start = obj_node.pos_start
		self.pos_end = value_node.pos_end
	
	def __repr__(self):
		return f'{self.obj_node}.{self.attr_name_tok} = {self.value_node}'

class ArrayAccessNode:
	def __init__(self, obj_node, index_node):
		self.obj_node = obj_node
		self.index_node = index_node
		self.pos_start = obj_node.pos_start
		self.pos_end = index_node.pos_end
	
	def __repr__(self):
		return f'{self.obj_node}[{self.index_node}]'

class ArrayAssignNode:
	def __init__(self, obj_node, index_node, value_node):
		self.obj_node = obj_node
		self.index_node = index_node
		self.value_node = value_node

		self.pos_start = obj_node.pos_start
		self.pos_end = value_node.pos_end
	
	def __repr__(self):
		return f'{self.obj_node}[{self.index_node}] = {self.value_node}'

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
	
	def __repr__(self):
		return f'ForNode({self.var_name_tok}, {self.start_value_node}, {self.end_value_node}, {self.step_value_node}, {self.body_node}, {self.should_return_null})'

class ForEachNode:
	def __init__(self, var_name_tok, iterable_node, body_node, should_return_null):
		self.var_name_tok = var_name_tok
		self.iterable_node = iterable_node
		self.body_node = body_node
		self.should_return_null = should_return_null

		self.pos_start = self.var_name_tok[0].pos_start
		self.pos_end = self.body_node.pos_end

class WhileNode:
	def __init__(self, condition_node, body_node, should_return_null):
		self.condition_node = condition_node
		self.body_node = body_node
		self.should_return_null = should_return_null

		self.pos_start = self.condition_node.pos_start
		self.pos_end = self.body_node.pos_end
	
	def __repr__(self):
		return f'WhileNode({self.condition_node}, {self.body_node}, {self.should_return_null})'

class VarAccessNode:
	def __init__(self, var_name_tok):
		self.var_name_tok = var_name_tok

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.var_name_tok.pos_end
	
	def __repr__(self):
		return f'VarAccessNode({self.var_name_tok})'

class VarAssignNode:
	def __init__(self, var_name_tok, value_node, constant=False, change_value=False, private=False, type_=None):
		self.var_name_toks = var_name_tok
		self.value_node = value_node
		self.constant = constant
		self.change_value = change_value	
		self.private = private
		self.type = type_

		if not isinstance(self.var_name_toks, list):
			self.var_name_toks = [var_name_tok]

		self.pos_start = self.var_name_toks[0].pos_start
		self.pos_end = self.value_node.pos_end if self.value_node else self.var_name_toks[-1].pos_end

	def __repr__(self):
		return f'VarAssignNode({self.var_name_toks}, {self.value_node})'

class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end if self.right_node else self.left_node.pos_end

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
	
	def __repr__(self):
		return f'ReturnNode({self.node_to_return})'

class ContinueNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f'ContinueNode()'

class BreakNode:
	def __init__(self, pos_start, pos_end):
		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f'BreakNode()'

class CheckNode:
	def __init__(self, condition, error_to_raise, details, pos_start, pos_end):
		self.condition_node = condition
		self.error_name = error_to_raise
		self.details = details
		
		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f'CheckNode({self.condition_node}, {self.error_name}, {self.details})'

class RaiseNode:
	def __init__(self, error_name, details, pos_start, pos_end):
		self.error_name = error_name
		self.details = details
		
		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f'RaiseNode({self.error_name}, {self.details})'

class TryCatchNode:
	def __init__(self, try_body, catch_var_tok, catch_body):
		self.try_body = try_body
		self.catch_var_tok = catch_var_tok
		self.catch_body = catch_body
		
		self.pos_start = self.try_body.pos_start
		self.pos_end = self.catch_body.pos_end
	
	def __repr__(self):
		return f'TryCatchNode({self.try_body}, {self.catch_var_tok}, {self.catch_body})'

class ImportNode:
	def __init__(self, file_path, functions, pos_start, pos_end):
		self.file_path = file_path
		self.functions = functions
		
		self.pos_start = pos_start
		self.pos_end = pos_end
	
	def __repr__(self):
		return f'ImportNode({self.file_path}, {self.functions})'

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
		
		if not isinstance(self.current_tok, Token):
			raise SyntaxError(f"Token inválido: {self.current_tok}")

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

			return_values = []

			expr = res.try_register(self.expr())
			if not expr:
				self.reverse(res.to_reverse_count)

			return_values.append(expr)

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				expr = res.register(self.expr())
				if res.error: return res

				return_values.append(expr)
			return res.success(ReturnNode(return_values, pos_start, self.current_tok.pos_start.copy()))
	
		elif self.current_tok.matches(TT_KEYWORD, 'conferir'):
			self.advance(res)

			condition = res.register(self.expr())
			if res.error: return res

			error_to_raise = None
			details = None

			if self.current_tok.matches(TT_KEYWORD, 'usando'):
				self.advance(res)

				error_to_raise = res.register(self.expr())
				if res.error: return res

			if self.current_tok.matches(TT_KEYWORD, 'detalhes'):
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
	
	def try_expr(self):
		res = ParseResult()

		if not self.current_tok.matches(TT_KEYWORD, 'tentar'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'tentar'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			try_body = res.register(self.statements())
			if res.error: return res
		elif self.current_tok.type == TT_ARROW:
			res.register_advancement()
			self.advance()

			try_body = res.register(self.statement())
			if res.error: return res
		else:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'nova	linha' ou '=>'"
			))

		if not self.current_tok.matches(TT_KEYWORD, 'caso'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'caso'"
			))
		
		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se um identificador"
			))
		
		catch_var_name = self.current_tok
		res.register_advancement()
		self.advance()

		if not self.current_tok.matches(TT_KEYWORD, 'faça'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'faça'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			exception_body = res.register(self.statements())
			if res.error: return res
		elif self.current_tok.type == TT_ARROW:
			res.register_advancement()
			self.advance()

			exception_body = res.register(self.statement())
			if res.error: return res
		else:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'nova	linha' ou '=>'"
			))

		if not self.current_tok.matches(TT_KEYWORD, 'fim'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'fim'"
			))

		res.register_advancement()
		self.advance()

		return res.success(TryCatchNode(try_body, catch_var_name, exception_body))
	
	def switch_expr(self):
		res = ParseResult()
		pos_start = self.current_tok.pos_start.copy()

		if not self.current_tok.matches(TT_KEYWORD, 'comparar'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'comparar'"
			))

		res.register_advancement()
		self.advance()

		expr_to_compare = res.register(self.expr())
		if res.error: return res

		if self.current_tok.type != TT_NEWLINE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se nova linha"
			))

		res.register_advancement()
		self.advance()

		cases = []
		default_case = None

		while self.current_tok.matches(TT_KEYWORD, 'caso'):
			case_start_pos = self.current_tok.pos_start.copy()
			
			res.register_advancement()
			self.advance()

			case_value = res.register(self.expr())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'faça'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Esperava-se 'faça'"
				))
			
			res.register_advancement()
			self.advance()

			if self.current_tok.type == TT_NEWLINE:
				res.register_advancement()
				self.advance()

				body = res.register(self.statements())
				if res.error: return res
				cases.append((case_value, body, True, case_start_pos))
			else:
				expr = res.register(self.statement())
				if res.error: return res
				cases.append((case_value, expr, False, case_start_pos))

		if self.current_tok.matches(TT_KEYWORD, 'senão'):
			else_start_pos = self.current_tok.pos_start.copy()
			res.register_advancement()
			self.advance()

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
				default_case = (statements, True, else_start_pos)
			else:
				expr = res.register(self.statement())
				if res.error: return res
				default_case = (expr, False, else_start_pos)
			
			

		if not self.current_tok.matches(TT_KEYWORD, 'fim'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'fim'"
			))

		res.register_advancement()
		self.advance()

		return res.success(SwitchNode(
			expr_to_compare, 
			cases, 
			default_case,
			pos_start,
			self.current_tok.pos_end.copy()
		))
			
	def atom(self):
		res = ParseResult()
		tok = self.current_tok
		pos_start = self.current_tok.pos_start.copy()

		if tok.type in (TT_INT, TT_FLOAT):
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))

		elif self.current_tok.matches(TT_KEYWORD, 'verdadeiro') or self.current_tok.matches(TT_KEYWORD, 'falso'):
			self.advance(res)
			return res.success(BoolNode(tok))
		
		elif tok.type == TT_STRING:
			parts = []
			pos_start = tok.pos_start.copy()
			
			for part_type, part_value in tok.parts[0]:
				if part_type == 'TEXT':
					parts.append(StringNode(Token(TT_STRING, part_value, pos_start, pos_start)))
				elif part_type == 'EXPR':
					expr_parser = Parser(part_value)
					expr_node = expr_parser.parse()
					if expr_node.error:
						return res.failure(expr_node.error)
					parts.append(expr_node.node)
			
			self.advance(res)
			return res.success(FStringNode(parts, pos_start, tok.pos_end))
		
		elif tok.type == TT_LBRACE:
			dict_expr = res.register(self.dict_expr())
			if res.error: return res
			return res.success(dict_expr)

		elif tok.type == TT_IDENTIFIER:
			res.register_advancement()
			self.advance()

			if self.current_tok.type in (TT_EQ, TT_ARROW, TT_PE, TT_ME):
				current_tok_type = self.current_tok.type
				res.register_advancement()
				self.advance()
				value = res.register(self.expr())
				if res.error: return res

				if current_tok_type in (TT_PE, TT_ME):
					target = tok
					op_type = TT_PLUS if current_tok_type == TT_PE else TT_MINUS
					op_tok = Token(op_type, pos_start=target.pos_start.copy(), pos_end=value.pos_end.copy())
					bin_op_node = BinOpNode(VarAccessNode(tok), op_tok, value)
					bin_op_node.pos_start = target.pos_start
					bin_op_node.pos_end = value.pos_end
					value = bin_op_node

				return res.success(VarAssignNode(tok, value, change_value=True))
			else:
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
		
		elif tok.matches(TT_KEYWORD, 'para'):
			for_each_expr = res.register(self.for_each_expr())
			if res.error: return res
			return res.success(for_each_expr)
		
		elif tok.matches(TT_KEYWORD, 'classe'):
			class_def = res.register(self.class_def())
			if res.error: return res
			return res.success(class_def)
		
		elif tok.matches(TT_KEYWORD, 'tentar'):
			try_expr = res.register(self.try_expr())
			if res.error: return res
			return res.success(try_expr)
		
		elif tok.matches(TT_KEYWORD, 'comparar'):
			switch_expr = res.register(self.switch_expr())
			if res.error: return res
			return res.success(switch_expr)

		elif tok.type == TT_LPAREN:
			pos_start = tok.pos_start.copy()
			res.register_advancement()
			self.advance()
			elements = []

			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(TupleNode(elements, pos_start, self.current_tok.pos_end.copy()))

			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			
			elif self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()
				elements.append(expr)
				elements.append(res.register(self.expr()))
				if res.error: return res

				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()
					elements.append(res.register(self.expr()))
					if res.error: return res

				if self.current_tok.type != TT_RPAREN:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se ')'"
					))
				res.register_advancement()
				self.advance()

				return res.success(TupleNode(elements, pos_start, self.current_tok.pos_end.copy()))
			else:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ')' ou ','"
				))
		
		elif tok.type == TT_LBRACKET:
			list_expr = res.register(self.list_expr())
			if res.error: return res
			return res.success(list_expr)
		
		elif tok.matches(TT_KEYWORD, 'super'):
			pos_start = tok.pos_start.copy()
			
			self.advance(res)

			return res.success(SuperNode(pos_start, self.current_tok.pos_end.copy()))

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
			if self.current_tok.type == TT_NEWLINE:
				self.advance(res)
			element_nodes.append(res.register(self.expr()))
			if res.error:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Syntaxe Inválida"
				))

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				
				if self.current_tok.type == TT_NEWLINE:
					self.advance(res)

				element_nodes.append(res.register(self.expr()))
				if res.error: return res

			
			if self.current_tok.type == TT_NEWLINE:
				self.advance(res)

			if self.current_tok.type != TT_RBRACKET:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ']' ou ','"
				))

			res.register_advancement()
			self.advance()
			return res.success(ListNode(element_nodes, pos_start, self.current_tok.pos_end.copy()))
	
	def set_expr(self):
		res = ParseResult()
		elements = []
		pos_start = self.current_tok.pos_start.copy()

		while True:
			if self.current_tok.type == TT_NEWLINE:
				self.advance(res)
			
			if self.current_tok.type == TT_RBRACE:
				return res.failure(InvalidSyntaxError(
					pos_start, self.current_tok.pos_end.copy(),
					"Não se pode ter uma Mesa vazia, esperava-se elemento (depois de ',' ou '{')\nSe está tentando criar um dicionário, faltou um identificador."
				))
			
			expr = res.register(self.expr())
			if res.error: return res
			elements.append(expr)

			if self.current_tok.type == TT_COMMA:
				self.advance(res)
				continue

			if self.current_tok.type == TT_NEWLINE:
				self.advance(res)

			if not self.current_tok.type == TT_RBRACE:
				return res.failure(InvalidSyntaxError(
					pos_start, self.current_tok.pos_end.copy(),
					"Esperava-se um '}' após fim de Mesa."
				))
			
			self.advance(res)

			return res.success(SetNode(elements, pos_start, self.current_tok.pos_end.copy()))
	
	def dict_expr(self):
		res = ParseResult()
		pairs = []
		pos_start = self.current_tok.pos_start.copy()
		
		res.register_advancement()
		self.advance()
		
		if self.current_tok.type == TT_NEWLINE:
			self.advance(res)
		
		if self.current_tok.type == TT_RBRACE:
			res.register_advancement()
			self.advance()
			return res.success(DictNode([], pos_start, self.current_tok.pos_end.copy()))
		
		while True:
			if self.current_tok.type == TT_NEWLINE:
				self.advance(res)

			if not self.current_tok.type == TT_IDENTIFIER:
				set_expr = res.register(self.set_expr())
				if res.error: return res
				return res.success(set_expr)
			
			key = self.current_tok
			if res.error: return res

			self.advance(res)
			
			if self.current_tok.type != TT_COLON:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se ':' depois de 'chave' de um dicionário"
				))
			
			res.register_advancement()
			self.advance()
			
			value = res.register(self.expr())
			if res.error: return res
			
			pairs.append((key, value))
			
			if self.current_tok.type != TT_COMMA:
				break
				
			res.register_advancement()
			self.advance()
		
		
		if self.current_tok.type == TT_NEWLINE:
			self.advance(res)
		
		if self.current_tok.type != TT_RBRACE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se ',' ou '}'"
			))
		
		res.register_advancement()
		self.advance()
		return res.success(DictNode(pairs, pos_start, self.current_tok.pos_end.copy()))


	def class_def(self):
		res = ParseResult()
		
		if not self.current_tok.matches(TT_KEYWORD, 'classe'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'classe'"
			))
		
		res.register_advancement()
		self.advance()
		
		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se um identificador para o nome da classe"
			))
		
		class_name_tok = self.current_tok
		res.register_advancement()
		self.advance()

		parent_name = None
		if self.current_tok.matches(TT_KEYWORD, 'extende'):
			self.advance(res)

			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se um identificador para o nome da classe pai"
				))
			
			parent_name = self.current_tok
			res.register_advancement()
			self.advance()
		
		if self.current_tok.type != TT_NEWLINE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se nova linha"
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
		
		return res.success(ClassNode(class_name_tok, body, parent_name))

	def func_def(self):
		res = ParseResult()
		if not self.current_tok.matches(TT_KEYWORD, 'função'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				'Esperava-se "função"'
			))
		
		self.advance(res)
		is_private = False

		if self.current_tok.matches(TT_KEYWORD, 'privada'):
			self.advance(res)
			is_private = True

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
		default_value_pairs = []

		if self.current_tok.type == TT_IDENTIFIER:
			tok = self.current_tok
			arg_name_toks.append(self.current_tok)
			self.advance(res)

			if self.current_tok.type == TT_EQ:
					self.advance(res)
					default_value_pairs.append((tok.value, res.register(self.expr())))
					if res.error: return res
			else:
				default_value_pairs.append((None, None))

			while self.current_tok.type == TT_COMMA:
				self.advance(res)
				if not self.current_tok.type == TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						'Esperava-se um identificador'
					))
				
				arg_name_toks.append(self.current_tok)
				tok = self.current_tok
				self.advance(res)

				if self.current_tok.type == TT_EQ:
					self.advance(res)
					default_value_pairs.append((tok.value, res.register(self.expr())))
					if res.error: return res
				else:
					default_value_pairs.append((None, None))


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

			return res.success(FuncDefNode(var_name_tok, arg_name_toks, node_to_return, True, 
		is_private, default_vals=default_value_pairs))
		
		elif self.current_tok.type != TT_NEWLINE:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se '->' ou NOVA_LINHA"
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
		False,
		is_private,
		default_vals=default_value_pairs
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

		if not self.current_tok.type in (TT_EQ, TT_ARROW) and not self.current_tok.matches(TT_KEYWORD, 'de'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se '=', '=>' ou 'de' após o identificador"
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
	
	def for_each_expr(self):
		res = ParseResult()
		
		if not self.current_tok.matches(TT_KEYWORD, 'para'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'para'"
			))

		self.advance()
		res.register_advancement()

		if not self.current_tok.matches(TT_KEYWORD, 'cada'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'cada' após 'para'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Esperava-se identificador para variável de iteração"
			))

		var_name_tok = [self.current_tok]
		res.register_advancement()
		self.advance()

		while self.current_tok.type == TT_COMMA:
			res.register_advancement()
			self.advance()

			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se identificador para variável de iteração após ','"
				))

			var_name_tok.append(self.current_tok)
			res.register_advancement()
			self.advance()

		if not self.current_tok.matches(TT_KEYWORD, 'em'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'em' após identificador"
			))

		res.register_advancement()
		self.advance()

		iterable_node = res.register(self.expr())
		if res.error: return res

		if not self.current_tok.matches(TT_KEYWORD, 'faça'):
			return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Esperava-se 'faça'"
			))

		res.register_advancement()
		self.advance()

		if self.current_tok.type == TT_NEWLINE:
			res.register_advancement()
			self.advance()

			body_node = res.register(self.statements())
			if res.error: return res

			if not self.current_tok.matches(TT_KEYWORD, 'fim'):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Esperava-se 'fim'"
				))

			res.register_advancement()
			self.advance()
			return res.success(ForEachNode(var_name_tok, iterable_node, body_node, True))
		else:
			body_node = res.register(self.statement())
			if res.error: return res
			return res.success(ForEachNode(var_name_tok, iterable_node, body_node, False))

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
					self.current_tok.pos_start, self.current_tok.pos_end,
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
		node = res.register(self.atom())
		if res.error: return res

		while True:
			if self.current_tok.type == TT_LPAREN:
				res.register_advancement()
				self.advance()
				arg_nodes = []

				if self.current_tok.type == TT_RPAREN:
					res.register_advancement()
					self.advance()
				else:
					arg_nodes.append(res.register(self.expr()))
					if res.error: return res

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
				node = CallNode(node, arg_nodes)
			elif self.current_tok.type == TT_DOT:
				res.register_advancement()
				self.advance()

				if self.current_tok.type != TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se um identificador"
					))

				attr_name = self.current_tok
				res.register_advancement()
				self.advance()
				node = AttrAccessNode(node, attr_name)
			elif self.current_tok.type == TT_LBRACKET:
				# array sla
				res.register_advancement()
				self.advance()

				index_expr = res.register(self.expr())
				if res.error: return res

				if self.current_tok.type != TT_RBRACKET:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se ']'"
					))

				res.register_advancement()
				self.advance()
				node = ArrayAccessNode(node, index_expr)
			else:
				break
		
		if self.current_tok.type == TT_EQ:
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if isinstance(node, AttrAccessNode):
				node = AttrAssignNode(node.obj_node, node.attr_name_tok, expr)
			elif isinstance(node, ArrayAccessNode):
				node = ArrayAssignNode(node.obj_node, node.index_node, expr)

		return res.success(node)

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
		
		node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_GT, TT_GTE, TT_LT, TT_LTE, (TT_KEYWORD, 'igual'), (TT_KEYWORD, 'em'))))
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
		is_private = False

		var_name_toks = []
		type_ = None

		if self.current_tok.matches(TT_KEYWORD, 'declarar'):
			res.register_advancement()
			self.advance()

			if self.current_tok.matches(TT_KEYWORD, 'constante'):
				res.register_advancement()
				self.advance()
				constant = True

			if self.current_tok.matches(TT_KEYWORD, 'privada'):
				self.advance(res)
				is_private = True

			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se um identificador"
				))

			var_name = self.current_tok
			res.register_advancement()
			self.advance()

			var_name_toks.append(var_name)

			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()

				if self.current_tok.type != TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Esperava-se um identificador"
					))

				var_name = self.current_tok
				var_name_toks.append(var_name)
				res.register_advancement()
				self.advance()
			
			if self.current_tok.type == TT_COLON:
				self.advance(res)
				type_ = self.current_tok
				self.advance(res)

			if self.current_tok.type == TT_NEWLINE:
				return res.success(VarAssignNode(var_name, None, constant=constant, private=is_private, type_=type_))
			elif not (self.current_tok.matches(TT_KEYWORD, 'como') or self.current_tok.type in (TT_EQ, TT_ARROW)):
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Esperava-se '=' ou '=>' ou ';'",
				))

			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			return res.success(VarAssignNode(var_name_toks, expr, constant=constant, private=is_private, type_=type_))

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
	executable = False
	properties = {}

	def __init__(self):
		self.executable = False
		self.properties = {}
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
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None

	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None

	def notted(self):
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def included_in(self, other):
		return None, self.illegal_operation(other)

	def execute(self, args):
		return RTResult().failure(self.illegal_operation(exec_=True))

	def copy(self):
		raise Exception('Nenhum ".copy()" metódo encontrado -> relatar esse erro ao dev.')

	def is_true(self):
		return False

	def illegal_operation(self, other=None, exec_=None):
		if not other: other = self
		if not other.pos_end: other.pos_end = self.pos_end
		if exec_:
			return RTError(
				self.pos_start, other.pos_end,
				f'Operação ilegal, tentativa de execução de {self}',
				self.context
			)
		return RTError(
			self.pos_start, other.pos_end,
			f'Operação Ilegal entre {repr(self)} e {repr(other)}',
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
		return super().added_to(other)

	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None
		return super().added_to(other)

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None
		return super().added_to(other)

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0:
				return None, RTError(
					other.pos_start, other.pos_end,
					f'Divisão por zero ({other}) não pode ser aceita',
					self.context
				)

			return Number(self.value / other.value).set_context(self.context), None
		return super().added_to(other)

	def powed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value ** other.value).set_context(self.context), None
		return super().get_comparison_gt(other)

	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Boolean(int(self.value == other.value)).set_context(self.context), None
		else:
			return Boolean(0).set_context(self.context), None
	
	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Boolean(int(self.value != other.value)).set_context(self.context), None
		else:
			return Boolean(1).set_context(self.context), None
	
	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Boolean(int(self.value < other.value)).set_context(self.context), None
		return super().get_comparison_gt(other)

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Boolean(int(self.value <= other.value)).set_context(self.context), None
		return super().get_comparison_gt(other)

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Boolean(int(self.value > other.value)).set_context(self.context), None
		return super().get_comparison_gt(other)
	
	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Boolean(int(self.value >= other.value)).set_context(self.context), None
		return super().get_comparison_gt(other)

	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None
	
	def notted(self):
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def is_true(self):
		return (int(True if self.value > 0 else False))
	
	def included_in(self, other):
		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, Number) and elem.value == self.value:
					return Boolean(1).set_context(self.context), None
			return Boolean(0).set_context(self.context), None
		elif isinstance(other, String):
			return Boolean(int(str(self.value) in other.value)).set_context(self.context), None
		return Boolean(0).set_context(self.context), None
	
	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		copy.properties = self.properties
		return copy
	
	def __repr__(self):
		return str(self.value)

	def __str__(self):
		return repr(self)

class Boolean(Value):
	def __init__(self, value: Literal["verdadeiro", "falso", 1, 0]):
		self.value = value

		if self.value == 1: self.value = 'verdadeiro'
		elif self.value == 0: self.value = 'falso'

		self.set_pos()
		self.set_context()
	
	def copy(self):
		copy = Boolean(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		copy.properties = self.properties
		return copy

	def get_comparison_eq(self, other):
		return Boolean(int(self.is_true() == other.is_true())).set_context(self.context), None
	
	def get_comparison_ne(self, other):
		return Boolean(int(self.is_true() != other.is_true())).set_context(self.context), None
	
	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None
	
	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def notted(self):
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None

	def included_in(self, other):
		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, Boolean) and elem.value == self.value:
					return Boolean(1).set_context(self.context), None
			return Boolean(0).set_context(self.context), None
		elif isinstance(other, String):
			return Boolean(int(str(self.value).lower() in other.value.lower())).set_context(self.context), None
		return Boolean(0).set_context(self.context), None

	def is_true(self):
		return 1 if self.value == 'verdadeiro' else 0
	
	def __repr__(self):
		return str(self)
	
	def __str__(self):
		return str(self.value)

class Nulo(Value):
	def __init__(self):
		self.value = None
		super().__init__()
	
	def copy(self):
		copy = Nulo()
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		copy.properties = self.properties
		return copy
	
	def is_true(self):
		return False
	
	def get_comparison_eq(self, other):
		return Boolean(int(self.is_true() == other.is_true())).set_context(self.context), None
	
	def get_comparison_ne(self, other):
		return Boolean(int(self.is_true() != other.is_true())).set_context(self.context), None
	
	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None
	
	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def notted(self):
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def included_in(self, other):

		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, Nulo):
					return Boolean(1).set_context(self.context), None
				
		return Boolean(0).set_context(self.context), None
	
	def __repr__(self):
		return "Nulo"
	
	def __str__(self):
		return "nulo"

class Set(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements
	
	def copy(self):
		copy = Set(self.elements)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.properties = self.properties
		return copy
	
	def is_true(self):
		return True
	
	def get_comparison_eq(self, other):
		return Boolean(int(self.elements == other.elements)).set_context(self.context).set_pos(self.pos_start, self.pos_end), None
	
	def get_comparison_ne(self, other):
		return Boolean(int(self.elements != other.elements)).set_context(self.context).set_pos(self.pos_start, self.pos_end), None
	
	def get_comparison_eq(self, other):
		return Boolean(int(self.elements == other.elements)).set_context(self.context).set_pos(self.pos_start, self.pos_end), None
	
	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context).set_pos(self.pos_start, self.pos_end), None

	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context).set_pos(self.pos_start, self.pos_end), None
	
	def notted(self):
		return Boolean(int(not self.is_true())).set_context(self.context).set_pos(self.pos_start, self.pos_end), None
	
	def included_in(self, other):
		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, Set) and [str(x) for x in elem.elements] == [str(x) for x in self.elements]:
					return Boolean(1).set_context(self.context), None
					
		return Boolean(0).set_context(self.context), None

	def __repr__(self):
		return f'Mesa({", ".join(self.elements)})'
	
	def __str__(self):
		reprs = f'{", ".join([str(x) for x in self.elements])}'
		return "{" + reprs + "}"


Number.true = Boolean(1)
Number.false = Boolean(0)
Number.null = Nulo()

class String(Value):
	def __init__(self, value):
		super().__init__()
		if isinstance(value, list):
			self.parts = value
			self.value = self._combine_parts()
		else:
			self.value = str(value)
			self.parts = [('TEXT', self.value)]

	def _combine_parts(self):
		combined = ""
		for part_type, part_value in self.parts:
			if part_type == 'TEXT':
				combined += part_value
			elif part_type == 'EXPR':
				combined += str(part_value)
		return combined
	
	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value + str(other.value)).set_context(self.context), None
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
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None

	def get_comparison_eq(self, other):
		if isinstance(other, String):
			return Boolean(int(self.value == other.value)).set_context(self.context), None
		return Number.false, None
	
	def get_comparison_ne(self, other):
		if isinstance(other, String):
			return Boolean(int(self.value != other.value)).set_context(self.context), None
		return Number.true, None
		
	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		copy.properties = self.properties
		return copy

	def is_true(self):
		return len(self.value) > 0 and self.value
	
	def included_in(self, other):
		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, String) and elem.value == self.value:
					return Boolean(1).set_context(self.context), None
			return Boolean(0).set_context(self.context), None
		elif isinstance(other, String):
			return Boolean(int(self.value in other.value)).set_context(self.context), None
		return Boolean(0).set_context(self.context), None

	
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
			return Boolean(int(self.elements == other.elements)).set_context(self.context), None
		return Number.false, None
	
	def get_comparison_ne(self, other):
		if isinstance(other, List):
			return Boolean(int(self.elements != other.elements)).set_context(self.context), None
		return Number.true, None
	
	def notted(self):
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def _ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None
	
	def is_true(self):
		return len(self.elements) > 0
	
	def included_in(self, other):
		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, List) and [str(x) for x in elem.elements] == [str(x) for x in self.elements]:
					return Boolean(1).set_context(self.context), None
					
		return Boolean(0).set_context(self.context), None
	
	def copy(self):
		copy = List(self.elements)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		copy.properties = self.properties
		return copy
	def __str__(self):
		return f'{", ".join([repr(x) for x in self.elements])}'
	
	def __repr__(self):
		return f'[{", ".join([repr(x) for x in self.elements])}]'

class Tuple(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements

	def added_to(self, other):
		if isinstance(other, Tuple):
			return Tuple(self.elements + other.elements), None
		return None, self.illegal_operation(other)

	def get_comparison_eq(self, other):
		if isinstance(other, Tuple):
			return Number(int(self.elements == other.elements)), None
		return Number.false, None
	
	def is_true(self):
		return len(self.elements) > 0
	
	def ored_by(self, other):
		return Boolean(int(self.is_true() or other.is_true())).set_context(self.context), None
	
	def anded_by(self, other):
		return Boolean(int(self.is_true() and other.is_true())).set_context(self.context), None
	
	def notted(self):
		return Boolean(int(0 if self.is_true() else 1)).set_context(self.context), None
	
	def included_in(self, other):
		if isinstance(other, (List, Set, Tuple)):
			for elem in other.elements:
				if isinstance(elem, Tuple) and elem.elements == self.elements:
					return Boolean(1).set_context(self.context), None
		return Boolean(0).set_context(self.context), None

	def copy(self):
		new_tup = Tuple(self.elements)
		new_tup.set_pos(self.pos_start, self.pos_end)
		new_tup.set_context(self.context)
		new_tup.properties = self.properties
		return new_tup

	def __repr__(self):
		return f'({", ".join([str(e) for e in self.elements])})'

class Dict(Value):
	def __init__(self, elements):
		super().__init__()
		self.elements = elements  # [(k, v)...]
		
	def added_to(self, other):
		if isinstance(other, Dict):
			new_dict = self.copy()
			for key, value in other.elements:
				new_dict.elements.append((key.copy(), value.copy()))
			return new_dict, None
		return None, self.illegal_operation(other)
	
	def change_val(self, key, value):
		for i, (k, v) in enumerate(self.elements):
			if k.value == key.value:
				self.elements[i] = (k, value)
				return
	
	def copy(self):
		copy = Dict([(k, v.copy()) for k, v in self.elements])
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		copy.properties = self.properties
		return copy
	
	def __str__(self):
		pairs = [f"{str(k.value)}: {str(v.copy())}" for k, v in self.elements]
		return ", ".join(pairs)
	
	def __repr__(self):
		
		return "{" + self.__str__() + "}"
	
	def keys(self):
		return List([String(k.value) if isinstance(k.value, str) else Number(k.value) for k, v in self.elements])
	
	def values(self):
		return List([v.copy() for k, v in self.elements])
	
	def items(self):
		return List([self.values(), self.keys()])
	
	def update(self, other):
		
		if isinstance(other, Dict):
			for key, value in other.elements:
				found = False
				for i, (k, v) in enumerate(self.elements):
					if k.value == key.value:
						self.elements[i] = (k, value)
						found = True
						break
				if not found:
					self.elements.append((key, value))
			return self
		return None, self.illegal_operation(other)

class BaseFunction(Value):
	def __init__(self, name):
		super().__init__()
		self.executable = True
		self.name = name or "<anônima>"
	
	def generate_new_context(self):
		new_context = Context(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable(new_context.parent.symbol_table if new_context.parent else global_symbol_table)
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
		
		return res.success(Number.null)
	
	def populate_args(self, arg_names, args, exec_ctx):
		for i in range(len(args)):
			arg_name = arg_names[i]
			arg_value = args[i]
			try:
				arg_value.set_context(exec_ctx)
			except Exception as e:
				pass
			exec_ctx.symbol_table.set(arg_name, arg_value)
	
	def check_and_populate_args(self, arg_names, args, exec_ctx):
		res = RTResult()
		res.register(self.check_args(arg_names, args))
		if res.should_return(): return res
		self.populate_args(arg_names, args, exec_ctx)
		return res.success(Number.null)
	
	def __repr__(self):
		return f"<função {self.name} em {FILENAME}>"

class Class(BaseFunction):
	def __init__(self, name, parent=False):
		super().__init__(name)
		self.arg_names = []
		self.parent = parent
		self.symbol_table = SymbolTable(
			parent.symbol_table if parent else self.context.symbol_table if self.context else global_symbol_table
		)

	def execute(self, args):
		res = RTResult()
			
		instance = Instance(self).set_context(self.context).set_pos(self.pos_start, self.pos_end)
		instance.symbol_table = SymbolTable(self.symbol_table)
		instance.symbol_table.set("esse", instance) 
	
		constructor = self.symbol_table.get("principal")
		if constructor:
			res.register(constructor.execute([instance] + args))
			if res.should_return(): return res
		
		self.context.symbol_table.set(instance.class_.name, instance)
		
		return res.success(instance)

	def copy(self):
		copy = Class(self.name, self.parent)
		copy.symbol_table = self.symbol_table
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.properties = self.properties
		return copy

	def __str__(self):
		return f"<classe {self.name} em {FILENAME}>"

class Instance(Value):
	def __init__(self, class_):
		super().__init__()
		self.class_ = class_
		self.symbol_table = SymbolTable(parent=class_.parent.symbol_table if class_.parent else class_.symbol_table)
		if not self.symbol_table.get('esse'): self.symbol_table.set('esse', self)
		if not self.context:
			self.context = Context(f"instance_of_{self.class_.name}", parent=self.class_.name)
		self.context.symbol_table = self.symbol_table
		self.obj = None
		self.type_name = class_.name 

	def get(self, name):
		value = self.symbol_table.get(name)
		
		if value is None and self.class_:
			method = self.class_.symbol_table.get(name)
			if isinstance(method, Function):
				bound = method.copy()
				bound.obj = self
				return bound

		return value if value is not None else Nulo().set_context(self.context).set_pos(self.pos_start, self.pos_end)

	def set(self, name, value, constant=False):
		self.symbol_table.set(name, value, constant)

	def copy(self):
		copy = Instance(self.class_)
		copy.symbol_table = self.symbol_table
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.properties = self.properties
		return copy
	
	def execute(self, args):
		return self.class_.execute(args)
	
	def is_true(self):
		return True

	def get_comparison_eq(self, other):
		return Boolean(int(self is other)).set_context(self.context).set_pos(self.pos_start, self.pos_end)
	
	def get_comparison_ne(self, other):
		return Boolean(int(self is not other)).set_context(self.context).set_pos(self.pos_start, self.pos_end)

	def __repr__(self):
		return f"<instância de {self.class_.name}>"
	
	def __str__(self):
		return f"<instância de {self.class_.name} em {FILENAME}>"


class Function(BaseFunction):
	def __init__(self, name, body_node, arg_names, should_auto_return=True, arg_defaults=False):
		super().__init__(name)
		self.body_node = body_node
		self.arg_names = arg_names
		self.should_auto_return = should_auto_return
		self.obj = None
		self.arg_defaults = arg_defaults or []
	
	def set_obj(self, obj):
		self.obj = obj
		return obj

	def execute(self, args):
		res = RTResult()
		interpreter = Interpreter()

		if self.obj:
			new_context = Context(self.name, parent=self.obj.context)
			new_context.symbol_table = SymbolTable(self.obj.symbol_table)
			args = [self.obj] + args
		else:
			new_context = self.generate_new_context()
		
		evaluated_args = []
		for i in range(len(self.arg_names)):
			if i < len(args):
				evaluated_args.append(args[i])
			else:
				if i < len(self.arg_defaults) and self.arg_defaults[i][1] is not None:
					default_value = res.register(interpreter.visit(
						self.arg_defaults[i][1], new_context
					))
					if res.error: return res
					evaluated_args.append(default_value)
				else:
					return res.failure(RTError(
						self.pos_start, self.pos_end,
						f"Faltando argumento para '{self.arg_names[i]}'",
						new_context
					))
		

		res.register(self.check_and_populate_args(self.arg_names, evaluated_args, new_context))
		if res.should_return():
			return res

		value = res.register(interpreter.visit(self.body_node, new_context))

		if res.should_return():
			if res.func_return_value is not None:
				return res.success(res.func_return_value)
			return res

		return res.success(value if value else Number.null)

	def copy(self):
		copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return, self.arg_defaults)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.obj = self.obj
		copy.properties = self.properties
		return copy

class BuiltInFunction(BaseFunction):
	def __init__(self, name):
		super().__init__(name)
		self.obj = None
	
	def execute(self, args):
		res = RTResult()
		exec_ctx = self.generate_new_context()

		method_name = f'execute_{self.name}'
		method = getattr(self, method_name, self.no_visit_method)

		evaluated_args = None
		if hasattr(method, 'default_args'):
			evaluated_args = []
			for i in range(len(method.arg_names)):
				if i < len(args):
					evaluated_args.append(args[i])
				else:
					if i < len(method.default_args) and method.default_args[i] is not None:
						evaluated_args.append(method.default_args[i])
					else:
						return res.failure(RTError(
							self.pos_start, self.pos_end,
							f"Faltando argumento para '{self.arg_names[i]}'",
							exec_ctx
						))

		res.register(self.check_and_populate_args(method.arg_names, evaluated_args or args, exec_ctx))
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
		copy.obj = self.obj
		copy.properties = self.properties
		return copy

	def __repr__(self):
		return f"< função predefinida {self.name} em (<stdlib>) >"
	
	def execute_list_mudar(self, exec_ctx):
		lst = self.obj
		if not isinstance(lst, List):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{lst}' não é uma lista.",
				exec_ctx
			))
		
		index = exec_ctx.symbol_table.get('index')
		value = exec_ctx.symbol_table.get('value')

		try:
			lst.elements[int(index.value)] = value
			return RTResult().success(value)
		except:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Indice '{index}' fora do intervalo, index não existe.",
				exec_ctx
			))
	execute_list_mudar.arg_names = ['index', 'value']

	def execute_dict_extender(self, exec_ctx):
		dictA = self.obj
		dictB = exec_ctx.symbol_table.get('dictB')

		if not isinstance(dictA, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Primeiro argumento deve ser um dicionário",
				exec_ctx
			))

		if not isinstance(dictB, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Segundo argumento deve ser um dicionário",
				exec_ctx
			))

		dictA.values.update(dictB.values)
		return RTResult().success(Dict(dictA.values).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
	execute_dict_extender.arg_names = ['dictB']

	def execute_dict_mudar(self, exec_ctx):
		res = RTResult()
		key = exec_ctx.symbol_table.get('key')
		value = exec_ctx.symbol_table.get('value')

		if not isinstance(key, String):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				"Primeiro argumento deve ser uma string",
				exec_ctx
			))

		f = False
		for i, (k, v) in enumerate(self.obj.elements):
			if k.value == key.value:
				self.obj.elements[i] = (k, value)
				f = True
				break
		if not f:
			self.obj.elements.append((key, value))
		
		return RTResult().success(self.obj)

	execute_dict_mudar.arg_names = ['key', 'value']
		
	def execute_falar(self, exec_ctx):
		print(str(exec_ctx.symbol_table.get('value')))
		return RTResult().success(String(str(exec_ctx.symbol_table.get('value'))))
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
			return RTResult().success(Number(len(val.elements)).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
		elif isinstance(val, String):
			return RTResult().success(Number(len(val.value)).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{val}' não é uma lista ou uma string.",
				exec_ctx
			))
	execute_len.arg_names = ['value']

	def execute_list_procurar(self, exec_ctx):
		lst = self.obj
		val = exec_ctx.symbol_table.get('value')

		for i in lst.elements:
			if val.value == i.value:
				return RTResult().success(Number(lst.elements.index(i)))

		return RTResult().success(Number(-1))
	execute_list_procurar.arg_names = ['value']

	def execute_list_colocar(self, exec_ctx):
		lst = self.obj
		val = exec_ctx.symbol_table.get('value')
		index = exec_ctx.symbol_table.get('index')

		if index.value == -1:
			lst.elements.append(val)
		else:
			try:
				lst.elements.insert(int(index.value), val)
			except:
				return RTResult().failure(RTError(
					self.pos_start, self.pos_end,
					f"Indice '{index}' fora do intervalo, index não existe.",
					exec_ctx
				))

		return RTResult().success(lst)
	execute_list_colocar.arg_names = ['value', 'index']
	execute_list_colocar.default_args = [None, Number(-1)]

	def execute_list_estourar(self, exec_ctx):
		lst = self.obj
		index = exec_ctx.symbol_table.get('index')

		try:
			return RTResult().success(lst.elements.pop(int(index.value)))
		except:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Indice '{index}' fora do intervalo, index não existe.",
				exec_ctx
			))
	execute_list_estourar.arg_names = ['index']
	execute_list_estourar.default_args = [Number(-1)]

	def execute_perguntar(self, exec_ctx):
		text = input(exec_ctx.symbol_table.get('prompt').value)
		return RTResult().success(String(text))
	execute_perguntar.arg_names = ['prompt']
	execute_perguntar.default_args = [String('')]

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
		value = exec_ctx.symbol_table.get('value')

		if isinstance(value, Number):
			return RTResult().success(value)
		elif isinstance(value, (String, Boolean, Nulo)):
			try:
				return RTResult().success(Number(int(value.value)))
			except ValueError:
				return RTResult().failure(ValorError(
					self.pos_start, self.pos_end,
					f"'{value}' não pode ser convertido para número.",
					exec_ctx
				))
		elif isinstance(value, (List, Dict, Tuple, Set)):
			return RTResult().failure(ValorError(
				self.pos_start, self.pos_end,
				f"'{value}' não pode ser convertido para número.",
				exec_ctx
			))
		else:
			return RTResult().failure(ValorError(
				self.pos_start, self.pos_end,
				f"'{value}' não pode ser convertido para número.",
				exec_ctx
			))
	execute_número.arg_names = ['value']

	def execute_num(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number):
			return RTResult().success(Boolean(1))
		else:
			return RTResult().success(Number.false)
	execute_num.arg_names = ['value']
	
	def execute_str(self, exec_ctx):
		res = RTResult()
		val = exec_ctx.symbol_table.get('value')

		if isinstance(val, String):
			return res.success(val)
		elif isinstance(val, Number):
			return res.success(String(str(val.value)))
		elif isinstance(val, Boolean):
			return res.success(String(str(val.value)))
		elif isinstance(val, List):
			return res.success(String(str(val.elements)))
		elif isinstance(val, Dict):
			return res.success(String(str(val.elements)))
		elif isinstance(val, Tuple):
			return res.success(String(str(val.elements)))
		elif isinstance(val, Set):
			return res.success(String(str(val.elements)))
		elif isinstance(val, BaseFunction):
			return res.success(String(str(val)))
		elif isinstance(val, Nulo):
			return res.success(String(str(val.value)))
		else:
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"'{val}' não pode ser convertido para 'string'.",
				exec_ctx
			))

	execute_str.arg_names = ['value']
	
	def execute_string(self, exec_ctx):
		return RTResult().success(Boolean(int(isinstance(exec_ctx.symbol_table.get('value'), String))))
	execute_string.arg_names = ['value']

	def execute_type(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number):
			return RTResult().success(String('número'))
		elif isinstance(exec_ctx.symbol_table.get('value'), String):
			return RTResult().success(String('string'))
		elif isinstance(exec_ctx.symbol_table.get('value'), List):
			return RTResult().success(String('lista'))
		elif isinstance(exec_ctx.symbol_table.get('value'), Function):
			return RTResult().success(String('função'))
		elif isinstance(exec_ctx.symbol_table.get('value'), Dict):
			return RTResult().success(String('dicionário'))
		elif isinstance(exec_ctx.symbol_table.get('value'), Boolean):
			return RTResult().success(String(String(exec_ctx.symbol_table.get('value').value)))
		elif isinstance(exec_ctx.symbol_table.get('value'), Tuple):
			return RTResult().success(String('tupla'))
		elif isinstance(exec_ctx.symbol_table.get('value'), Nulo):
			return RTResult().success(String('nulo'))
		elif isinstance(exec_ctx.symbol_table.get('value'), (Set)):
			return RTResult().success(String('mesa'))
		elif isinstance(exec_ctx.symbol_table.get('value'), Instance):
			return RTResult().success(String(exec_ctx.symbol_table.get('value').class_.name))
		elif isinstance(exec_ctx.symbol_table.get('value'), Class):
			return RTResult().success(String(exec_ctx.symbol_table.get('value').name))
		else:
			return RTResult().success(Number.null)

	execute_type.arg_names = ['value']
	
	def execute_lista(self, exec_ctx):
		return RTResult().success(Boolean(int(isinstance(exec_ctx.symbol_table.get('value'), List))))
	execute_lista.arg_names = ['value']
	
	def execute_func(self, exec_ctx):
		return RTResult().success(Boolean(int(isinstance(exec_ctx.symbol_table.get('value'), Function))))
	execute_func.arg_names = ['value']

	def execute_string_picotar(self, exec_ctx):
		val = self.obj
		if isinstance(val, String):
			return RTResult().success(List(self.obj.value.split()))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{self.obj}' não é uma string.",
				exec_ctx
			))
	execute_string_picotar.arg_names = []

	def execute_string_limpar(self, exec_ctx):
		if isinstance(self.obj, String):
			return RTResult().success(List(self.obj.value.strip()))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{self.obj}' não é uma string.",
				exec_ctx
			))
	execute_string_limpar.arg_names = []

	def execute_list_extender(self, exec_ctx):
		listA = self.obj
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
	execute_list_extender.arg_names = ["listB"]

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

	def execute_string_fatiar(self, exec_ctx):
		value = self.obj
		
		if not isinstance(value, String):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{value}' não é uma string",
				exec_ctx
			))
		
		chars = [String(char) for char in value.value]
		return RTResult().success(List(chars).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
	execute_string_fatiar.arg_names = []

	def execute_string_minúsculo(self, exec_ctx):
		if isinstance(self.obj, String):
			return RTResult().success(String(self.obj.value.lower()).set_pos(self.pos_start, self.pos_end).set_context(exec_ctx))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{self.obj}' não é uma string.",
				exec_ctx
			))
	execute_string_minúsculo.arg_names = []
	
	def execute_string_maiúsculo(self, exec_ctx):
		if isinstance(self.obj, String):
			return RTResult().success(String(self.obj.value.upper()).set_pos(self.pos_start, self.pos_end).set_context(exec_ctx))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{self.obj}' não é uma string.",
				exec_ctx
			))
	execute_string_maiúsculo.arg_names = []

	def execute_aleatório(self, exec_ctx):
		if isinstance(exec_ctx.symbol_table.get('value'), Number) and isinstance(exec_ctx.symbol_table.get('max'), Number):
			return RTResult().success(Number(random.randint(int(exec_ctx.symbol_table.get('value').value), int(exec_ctx.symbol_table.get('max').value))))
		else:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"'{exec_ctx.symbol_table.get('value')}' ou '{exec_ctx.symbol_table.get('max')}' não é um número.",
				exec_ctx
			))
	execute_aleatório.arg_names = ['value', 'max']

	def execute_ler_arquivo(self, exec_ctx):
		caminho = exec_ctx.symbol_table.get('caminho').value
		try:
			with open(caminho, 'r', encoding='utf-8') as f:
				return RTResult().success(String(f.read()))
		except Exception as e:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Erro ao ler arquivo: {str(e)}",
				exec_ctx
			))
	execute_ler_arquivo.arg_names = ['caminho']

	def execute_escrever_arquivo(self, exec_ctx):
		caminho = exec_ctx.symbol_table.get('caminho').value
		conteúdo = exec_ctx.symbol_table.get('conteúdo').value
		try:
			with open(caminho, 'w', encoding='utf-8') as f:
				f.write(str(conteúdo))
			return RTResult().success(Number.true)
		except Exception as e:
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Erro ao escrever arquivo: {str(e)}",
				exec_ctx
			))
	execute_escrever_arquivo.arg_names = ['caminho', 'conteúdo']

	def execute_clicommand(self, exec_ctx):
		res = RTResult()
		comando = exec_ctx.symbol_table.get('comando')
		
		if not isinstance(comando, String):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				"Argumento deve ser uma string",
				exec_ctx
			))
		
		try:
			resultado = subprocess.run(
				comando.value,
				shell=True,
				capture_output=True,
				text=True,
				timeout=30,
				encoding='utf-8',
				errors='replace'
			)
			
			output = resultado.stdout.strip()
			if resultado.returncode != 0:
				return res.failure(RTError(self.pos_start, self.pos_end, f"\n[ERRO {resultado.returncode}] {resultado.stderr.strip()}"))
			
			return res.success(String(output))
			
		except subprocess.TimeoutExpired:
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				"Timeout: Comando excedeu 30 segundos",
				exec_ctx
			))
		except Exception as e:
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"Erro na execução: {str(e)}",
				exec_ctx
			))

	execute_clicommand.arg_names = ['comando']

	def execute_eval(self, exec_ctx):
		res = RTResult()
		comando = exec_ctx.symbol_table.get('comando')
		
		if not isinstance(comando, String):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				"Argumento deve ser uma string",
				exec_ctx
			))
		
		try:
			rs, err, exec_ctx = run('<eval_expr>', comando.value)
			if err: return res.failure(err)
			return res.success(rs)
		except Exception as e:
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"Erro na execução: {str(e)}",
				exec_ctx
			))

	execute_eval.arg_names = ['comando']

	def execute_python(self, exec_ctx):
		res = RTResult()
		comando = exec_ctx.symbol_table.get('comando')
		
		if not isinstance(comando, String):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				"Argumento deve ser uma string",
				exec_ctx
			))
		
		try:
			return res.success(eval(comando.value))
		except Exception as e:
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				f"Erro na execução: {str(e)}",
				exec_ctx
			))

	execute_python.arg_names = ['comando']

	def execute_list_invertida(self, exec_ctx):
		return RTResult().success(List(
			self.obj.elements[::-1]
		).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
	execute_list_invertida.arg_names = []

	def execute_dict_keys(self, exec_ctx):
		return RTResult().success(List(
			[String(k.value) for k, _ in self.obj.elements]
		))
	execute_dict_keys.arg_names = []

	def execute_dict_values(self, exec_ctx):
		return RTResult().success(List(
			[v.copy() for _, v in self.obj.elements]
		))
	execute_dict_values.arg_names = []

	def execute_dict_items(self, exec_ctx):
		return RTResult().success(List([
			List([String(k.value), v.copy()]) for k, v in self.obj.elements
		]))
	execute_dict_items.arg_names = []

	def execute_update(self, exec_ctx):
		obj = exec_ctx.symbol_table.get("obj")
		other = exec_ctx.symbol_table.get("other")

		if not isinstance(obj, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Primeiro argumento deve ser um dicionário",
				exec_ctx
			))

		if not isinstance(other, Dict):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				"Segundo argumento deve ser um dicionário",
				exec_ctx
			))

		obj.elements.extend((k, v) for k, v in other.elements)
		return RTResult().success(Dict(obj.elements).set_context(exec_ctx).set_pos(self.pos_start, self.pos_end))
	execute_update.arg_names = ['obj', 'other']

	def execute_esperar(self, exec_ctx):
		tempo = exec_ctx.symbol_table.get("time")
		if not isinstance(tempo, Number):
			return RTResult().failure(RTError(
				self.pos_start, self.pos_end,
				f"Tempo não é um número, mas sim {type(tempo)}",
				exec_ctx
			))

		time.sleep(float(tempo.value))
		return RTResult().success(Number.null)

	execute_esperar.arg_names = ["time"]
	execute_esperar.default_args = [Number(1)]

	def execute_bomba_exit(self, exec_ctx):
		res = RTResult()
		sys.exit(exec_ctx.symbol_table.get('value'))
	execute_bomba_exit.arg_names = ['value']
	execute_bomba_exit.default_args = [Number(0)]

	def execute_list_map(self, exec_ctx):
		res = RTResult()
		lista = self.obj
		func = exec_ctx.symbol_table.get('func')

		if not isinstance(func, BaseFunction):
			return res.failure(RTError(
				self.pos_start, self.pos_end,
				"Argumento deve ser uma função",
				exec_ctx
			))

		self.obj.elements = ([func.set_context(exec_ctx).execute([arg]).value for arg in lista.elements.copy()])
		if res.should_return(): return res
		return res.success(self.obj)

	execute_list_map.arg_names = ['func']

	def execute_any_is_callable(self, exec_ctx):
		return RTResult().success(Number.true if self.obj.executable else Number.false)
	execute_any_is_callable.arg_names = []
	
	def execute_is_callable(self, exec_ctx):
		return RTResult().success(Number.true if (exec_ctx.symbol_table.get('value').executable) else Number.false)

	execute_is_callable.arg_names = ['value']



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
	def __init__(self, parent=None, child=None):
		self.symbols = {}
		self.constants = []
		self.privates = []
		self.types = []
		self.parent = parent
		
	def get(self, name):
		value = self.symbols.get(name, None)
		if value == None and self.parent:
			value = self.parent.get(name)
		return value

	def set(self, name, value, constant=False, private=False, type_=None):
		if name in self.constants:
			return 'error'
		if self.parent and name in self.parent.constants:
			return 'error'
		
		self.symbols[name] = value
		if private: self.privates.append(name)
		if constant: self.constants.append(name)
		if type_: self.types.append((name, type_))
	
	def get_type(self, name):
		for var_name, type_token in self.types:
			if var_name == name:
				return type_token.value
		if self.parent:
			return self.parent.get_type(name)
		return None
	
	def check_type_compatibility(self, declared_type, actual_type):
		if declared_type == actual_type:
			return True
		
		actual_class = self.get(actual_type)
		declared_class = self.get(declared_type)

		if isinstance(actual_class, Instance) and isinstance(declared_class, Instance):
			current_parent = actual_class.class_.parent
			while current_parent:
				if current_parent.name == declared_type:
					return True
				current_parent = current_parent.parent
		return False
	
	def is_constant(self, name):
		val = name in self.constants or None
		if self.parent: val = val or self.parent.is_constant(name)
		return val

	def is_private(self, name):
		val = None
		if name in self.privates: return True
		if self.parent: val = val or self.parent.is_private(name)
		return val
	
	def copy(self):
		copy = SymbolTable()
		copy.symbols = self.symbols.copy()
		copy.parent = self.parent
		copy.constants = self.constants.copy()
		copy.properties = self.properties
		return copy

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
	
	def visit_BoolNode(self, node, context):
		return RTResult().success(
			Boolean(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		)
	
	def visit_SetNode(self, node, context):
		res = RTResult()
		elements = []

		for item in node.elements:
			elements.append(res.register(self.visit(item, context)))
			if res.should_return(): return res

		return res.success(
			Set(elements).set_context(context).set_pos(node.pos_start, node.pos_end)
		)
	
	def visit_FStringNode(self, node, context):
		res = RTResult()
		evaluated_parts = []
		
		for part in node.parts:
			if isinstance(part, StringNode):
				evaluated_parts.append(('TEXT', part.tok.value))
			else:
				value = res.register(self.visit(part, context))
				if res.error: return res
				evaluated_parts.append(('EXPR', value))
		
		return res.success(String(evaluated_parts))

	def visit_ClassNode(self, node: ClassNode, context):
		res = RTResult()
		
		parent_class = None
		if node.inheritance:
			parent_value = context.symbol_table.get(node.inheritance.value)
			if not isinstance(parent_value, Class):
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Superclasse '{node.inheritance.value}' não é uma classe válida",
					context
				))
			parent_class = parent_value
			
		class_value = Class(
			node.class_name_tok.value,
			parent_class
		)
		
		class_context = Context(class_value.name, parent=context)
		class_context.symbol_table = class_value.symbol_table

		res.register(self.visit(node.body_node, class_context))
		if res.error: return res
		
		context.symbol_table.set(class_value.name, class_value)
		return res.success(class_value)

	def visit_VarAccessNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
		value = context.symbol_table.get(var_name)

		if not value:
			if hasattr(context, 'symbol_table') and hasattr(context.symbol_table, 'parent') and context.symbol_table.parent:
				instance = context.symbol_table.parent.get('esse')
				if isinstance(instance, Instance):
					value = instance.get(var_name)
					if value:
						value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
						return res.success(value)
			
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"'{var_name}' nunca foi declarado",
				context
			))

		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		return res.success(value)

	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		
		value = res.register(self.visit(node.value_node, context)) if node.value_node else Nulo().set_context(context).set_pos(node.pos_start, node.pos_end)
		if res.should_return(): return res
		
		for var_tok in node.var_name_toks:
			var_name = var_tok.value
						
			
			if 'esse' in context.symbol_table.symbols and isinstance(context.symbol_table.get('esse'), Instance):
				instance = context.symbol_table.get('esse')
				instance.set(var_name, value.copy(), node.constant)
			else:
				result = context.symbol_table.set(var_name, value.copy(), node.constant, node.private, node.type)
				if result == 'error':
					return res.failure(RTError(
						node.pos_start, node.pos_end,
						f"'{var_name}' não pode ser alterado, já que é uma constante.",
						context
					))
		
		declared_type = context.symbol_table.get_type(var_name)
		if declared_type:
			actual_type = (
				value.type_name
				if isinstance(value, Instance)
				else value.__class__.__name__.lower()
			)
			
			if not context.symbol_table.check_type_compatibility(declared_type, actual_type):
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Tipo inválido para '{var_name}': esperado '{declared_type}', obtido '{actual_type}'.",
					context
				))

		value = value.copy().set_pos(node.pos_start, node.pos_end).set_context(context)
		
		return res.success(value)
	
	def visit_ListNode(self, node, context):
		res = RTResult()
		elements = []

		for element_node in node.element_nodes:
			elements.append(res.register(self.visit(element_node, context)))
			if res.should_return(): return res

		return res.success(List(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_TupleNode(self, node, context):
		res = RTResult()
		elements = []
		
		for element_node in node.element_nodes:
			elements.append(res.register(self.visit(element_node, context)))
			if res.error: return res
			
		return res.success(Tuple(elements))

	def visit_DictNode(self, node, context):
		res = RTResult()
		elements = []
		
		for key_node, value_node in node.pairs:
			key = String(key_node.value)
			
			if isinstance(value_node, (String, Number)):
				value = value_node
			else:
				value = res.register(self.visit(value_node, context))
			if res.error: return res
			
			elements.append((key, value))
		
		return res.success(Dict(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_AttrAccessNode(self, node, context: Context):
		res = RTResult()
		obj = res.register(self.visit(node.obj_node, context))
		if res.error: return res

		attr_name = node.attr_name_tok.value

		if attr_name == "pode_executar":
			func = BuiltInFunction("any_is_callable")
			func.obj = obj
			func.set_context(context)
			return res.success(func)

		if isinstance(obj, Dict):
			if attr_name in ["chaves", "valores", "itens", "mudar", "extender"]:
				if attr_name == "chaves": attr_name = "keys"
				elif attr_name == "valores": attr_name = "values"
				elif attr_name == "itens": attr_name = "items"
				func = BuiltInFunction(f"dict_{attr_name}")
				func.obj = obj
				func.set_context(context)
				return res.success(func)
			else:
				for key, value in obj.elements:
					if key.value == attr_name:
						return res.success(value.copy())
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Chave '{attr_name}' não encontrada.",	
					context
				))
		elif isinstance(obj, List):
			if attr_name in ["invertida", "procurar", "colocar", "map", "estourar", "extender", "mudar"]:
				func = BuiltInFunction(f"list_{attr_name}")
				func.obj = obj
				func.set_context(context)
				return res.success(func)
			else:
				if attr_name in obj.properties:
					return res.success(obj.properties[attr_name].copy())
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Atributo '{attr_name}' para listas não encontrado, talvez tenha confudido com os métodos de dicionários?",
					context
				))
		elif isinstance(obj, String):
			if attr_name in ["fatiar", "picotar", "limpar", "maiúsculo", "minúsculo"]:
				func = BuiltInFunction(f"string_{attr_name}")
				func.obj = obj
				func.set_context(context)
				return res.success(func)
			else:
				if attr_name in obj.properties:
					return res.success(obj.properties[attr_name].copy())
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Atributo '{attr_name}' para strings não encontrado, talvez tenha confudido com os métodos de dicionários?",
					context
				))
		elif isinstance(obj, Instance):
			inst = obj
			value = obj.get(attr_name)
			context.symbol_table.set('esse', inst, private=True)
			if isinstance(value, Function):
				value.set_obj(inst)
			return res.success(value)
		else:
			if attr_name in obj.properties:
				return res.success(obj.properties[attr_name].copy())
		

		return res.failure(RTError(
			node.pos_start, node.pos_end,
			"Attribute access only supported for dictionaries",
			context
		))
	
	def visit_AttrAssignNode(self, node, context):
		res = RTResult()
		
		obj = res.register(self.visit(node.obj_node, context))
		if res.error: return res
		
		value = res.register(self.visit(node.value_node, context))
		if res.error: return res
		
		attr_name = node.attr_name_tok.value

		if isinstance(obj, Instance):
			obj.set(attr_name, value)
			return res.success(value)
		elif isinstance(obj, Dict):
			found = False
			for i, (k, v) in enumerate(obj.elements):
				if k.value == attr_name:
					obj.elements[i] = (k, value)
					found = True
					break
			if not found:
				obj.elements.append((String(attr_name), value))
			return res.success(value)
		else:
			obj.properties[attr_name] = value
			return res.success(value)
	
	def visit_ArrayAccessNode(self, node, context):
		res = RTResult()
		
		obj = res.register(self.visit(node.obj_node, context))
		if res.error: return res
		
		index = res.register(self.visit(node.index_node, context))
		if res.error: return res
		
		if isinstance(obj, List):
			try:
				value = (obj.elements[int(index.value)])

				if isinstance(value, str):
					return res.success(String(value).set_context(context).set_pos(node.pos_start, node.pos_end))
				elif isinstance(value, int) or isinstance(value, float):
					return res.success(Number(value).set_context(context).set_pos(node.pos_start, node.pos_end))
				elif isinstance(value, list):
					for i in value:
						if len(i) < 2:
							return res.success(List(value).set_context(context).set_pos(node.pos_start, node.pos_end))
						else:
							return res.success(Dict(value).set_context(context).set_pos(node.pos_start, node.pos_end))
				else:
					return res.success(value.copy())
			except:
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Index {index} não existe",
					context
				))
		elif isinstance(obj, Dict):
			for k, v in obj.elements:
				if k.value == index.value:
					return res.success(v.copy())
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"Chave '{index}' não encontrada",
				context
			))
		elif isinstance(obj, Set):
			try:
				value = (obj.elements[int(index.value)])

				if isinstance(value, str):
					return res.success(String(value).set_context(context).set_pos(node.pos_start, node.pos_end))
				elif isinstance(value, int) or isinstance(value, float):
					return res.success(Number(value).set_context(context).set_pos(node.pos_start, node.pos_end))
				elif isinstance(value, list):
					for i in value:
						if len(i) < 2:
							return res.success(List(value).set_context(context).set_pos(node.pos_start, node.pos_end))
						else:
							return res.success(Dict(value).set_context(context).set_pos(node.pos_start, node.pos_end))
				else:
					return res.success(value.copy())
			except:
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Index {index} não existe",
					context
				))
		
		return res.failure(RTError(
			node.pos_start, node.pos_end,
			"Acesso usando [index] somente disponível para listas",
			context
		))
	
	def visit_SwitchNode(self, node, context):
		res = RTResult()
		
		value_to_compare = res.register(self.visit(node.expr_to_compare, context))
		if res.should_return(): return res

		for case_value, body_node, is_block, _ in node.cases:
			case_val = res.register(self.visit(case_value, context))
			if res.should_return(): return res

			result, error = value_to_compare.get_comparison_eq(case_val)
			if error: return res.failure(error)
			
			if result.is_true():
				body_value = res.register(self.visit(body_node, context))
				if res.should_return(): return res
				return res.success(body_value)

		if node.default_case:
			default_body, is_block, _ = node.default_case
			default_value = res.register(self.visit(default_body, context))
			if res.should_return(): return res
			return res.success(default_value)

		return res.success(Number.null)
	
	def visit_TryCatchNode(self, node, context):
		res = RTResult()
		try_result = res.register(self.visit(node.try_body, context))
		if res.loop_should_break or res.loop_should_continue or res.func_return_value: return res

		if not res.should_return():
			return res.success(try_result)
		
		error_val = String(str(res.error.as_string())).set_context(context).set_pos(node.pos_start, node.pos_end)

		res.reset()
		context.symbol_table.set(node.catch_var_tok.value, error_val)
		catch_result = res.register(self.visit(node.catch_body, context))
		if res.should_return(): return res
		
		return res.success(catch_result)

	def visit_CheckNode(self, node, context):
		res = RTResult()

		condition = res.register(self.visit(node.condition_node, context))
		if res.should_return(): return res

		if condition.is_true():
			return res.success(Number(1).set_context(context).set_pos(node.pos_start, node.pos_end))
		else:
			error_name = res.register(self.visit(node.error_name, context)) if node.error_name else None
			details = res.register(self.visit(node.details, context)) if node.details else "Expressão Conferir resultou em Falso."

			return res.failure(Error(
				node.pos_start, node.pos_end,
				str(error_name), repr(details)
			)) if node.error_name else res.failure(CheckError(
				node.pos_start, node.pos_end,
				repr(details)
			))
	
	def visit_RaiseNode(self, node, context):
		res = RTResult()

		error_name = res.register(self.visit(node.error_name, context))
		details = res.register(self.visit(node.details, context))

		return res.failure(Error(
			node.pos_start, node.pos_end,
			repr(error_name), repr(details)
		))
	
	def visit_SuperNode(self, node, context):
		res = RTResult()
		instance = context.symbol_table.get('esse')
		
		if not isinstance(instance, Instance):
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				"'super()' só pode ser usado dentro de métodos de classe",
				context
			))
		
		if not instance.class_.parent:
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				"A classe não possui uma superclasse",
				context
			))

		return res.success(instance.class_.parent.set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_ImportNode(self, node, context):
		res = RTResult()
	
		file_path = node.file_path
		module_name = file_path.value
		
		current_dir = sys.path[0]
		full_path = os.path.join(current_dir, 'modules', f'{module_name}.modneb')
		if not os.path.exists(full_path):
			full_path = os.path.join(current_dir, f'../{module_name}.neb')
		

		
		if not os.path.exists(full_path):
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"Module '{module_name}' em caminho '{full_path}' não encontrado.",
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
		
		for func_tok in node.functions:
			func_name = func_tok.value
			func_value = module_ctx.symbol_table.get(func_name)
			func_const = module_ctx.symbol_table.is_constant(func_name)

			if module_ctx.symbol_table.is_private(func_name):
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Function '{func_name}' is private and cannot be imported",
					context
				))
			if not func_value:
				return res.failure(RTError(
					node.pos_start, node.pos_end,
					f"Function '{func_name}' not found in module '{module_name}'",
					context
				))
			context.symbol_table.set(func_name, func_value, func_const)
		
		return res.success(Number.null)


	def visit_FuncDefNode(self, node, context):
		res = RTResult()

		func_name = node.var_name_tok.value if node.var_name_tok else None
		body_node = node.body_node
		arg_names = [arg_name.value for arg_name in node.arg_name_toks]
		func_value = Function(func_name, body_node, arg_names, node.should_auto_return, arg_defaults=node.default_vals).set_context(context).set_pos(node.pos_start, node.pos_end)
		func_value.arg_defaults = [node.default_vals[i] for i in range(len(node.default_vals))]

		if node.var_name_tok:
			context.symbol_table.set(func_name, func_value, private=node.private)

		return res.success(func_value)
	
	def visit_CallNode(self, node, context):		
		res = RTResult()
		args = []

		value_to_call = res.register(self.visit(node.node_to_call, context))
		if res.should_return(): return res
		if not isinstance(value_to_call, (BaseFunction, Class, Instance)):
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"'{value_to_call}' não pode ser executado",
				context
			))
		value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.should_return(): return res

		return_value = res.register(value_to_call.execute(args))
		if res.should_return(): return res

		return res.success(return_value)
	
	def visit_IfNode(self, node, context):
		res = RTResult()

		expr_value = Number.null

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
		elif node.op_tok.matches(TT_KEYWORD, 'em'):
			result, error = left.included_in(right)

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

	
	def visit_ForEachNode(self, node, context):
		res = RTResult()
		elements = []
		
		iterable = res.register(self.visit(node.iterable_node, context))
		if res.error: return res
		
		var_names = [tok.value for tok in node.var_name_tok]
		
		if not isinstance(iterable, (List, Tuple, Set, Dict)):
			return res.failure(RTError(
				node.pos_start, node.pos_end,
				"Iterável deve ser lista, tuple, string ou dicionário",
				context
			))
			
		items = iterable.elements
		single = False

		if isinstance(iterable, Tuple):
			for item in items:
				if len(var_names) == 1:
					context.symbol_table.set(var_names[0], item.copy())
				else:
					for var_name, element in zip(var_names, items):
						context.symbol_table.set(var_name, element.copy())
						single = True
				
				value = res.register(self.visit(node.body_node, context))
				if res.should_return() and not res.loop_should_continue and not res.loop_should_break:
					return res
					
				if res.loop_should_continue:
					continue
					
				if res.loop_should_break:
					break

				if single == True:
					elements.append(value)
					break
					
				elements.append(value)
		else:
			for item in items:
				if isinstance(item, tuple) and isinstance(iterable, Dict) and len(var_names) == len(item):
					for var_name, element in zip(var_names, item):
						context.symbol_table.set(var_name, element.copy())
				elif isinstance(iterable, Dict):
					for var_name in var_names:
						context.symbol_table.set(var_name, Tuple(item).set_context(context).set_pos(node.pos_start, node.pos_end))
				elif isinstance(item, Tuple) and len(var_names) == len(item.elements):
					for var_name, element in zip(var_names, item.elements):
						context.symbol_table.set(var_name, element.copy())
				else:
					for name in var_names:
						context.symbol_table.set(name, item.copy())
				
				value = res.register(self.visit(node.body_node, context))
				if res.should_return() and not res.loop_should_continue and not res.loop_should_break:
					return res
					
				if res.loop_should_continue:
					continue
					
				if res.loop_should_break:
					break
					
				elements.append(value)
		
		return res.success(Number.null if node.should_return_null else List(elements))


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
		values = []

		if node.node_to_return and node.node_to_return != [None]:
			for value_node in node.node_to_return:
				values.append(res.register(self.visit(value_node, context) if value_node else Nulo().set_context(context).set_pos(node.pos_start, node.pos_end)))
				if res.should_return(): return res

			return_value = List(values) if len(values) > 1 else values[0] if values else Number.null

			return res.success_return(return_value)
		else:
			return res.success_return(Nulo().set_context(context).set_pos(node.pos_start, node.pos_end))
	
	def visit_ContinueNode(self, node, context):
		return RTResult().success_continue()
	
	def visit_BreakNode(self, node, context):
		return RTResult().success_break()

#######################################
# RUN
#######################################

global_symbol_table = SymbolTable()
global_symbol_table.set("nulo", Number.null, True)
global_symbol_table.set("falar", BuiltInFunction("falar"), True)
global_symbol_table.set("pegar", BuiltInFunction("pegar"), True)
global_symbol_table.set("perguntar", BuiltInFunction("perguntar"), True)
global_symbol_table.set("input_int", BuiltInFunction("input_int"), True)
global_symbol_table.set("extender", BuiltInFunction("extender"), True)
global_symbol_table.set("int", BuiltInFunction("número"), True)
global_symbol_table.set("raiz", BuiltInFunction("raiz"), True)
global_symbol_table.set("cubo", BuiltInFunction("cubo"), True)
global_symbol_table.set("quadrado", BuiltInFunction("quadrado"), True)
global_symbol_table.set("str", BuiltInFunction("str"), True)
global_symbol_table.set("len", BuiltInFunction("len"), True)
global_symbol_table.set("é_string", BuiltInFunction("string"), True)
global_symbol_table.set("é_número", BuiltInFunction("num"), True)
global_symbol_table.set("picotar", BuiltInFunction("picotar"), True)
global_symbol_table.set("lista", BuiltInFunction("lista"), True)
global_symbol_table.set("func", BuiltInFunction("func"), True)
global_symbol_table.set("procurar", BuiltInFunction("procurar"), True)
global_symbol_table.set("colocar", BuiltInFunction("colocar"), True)
global_symbol_table.set("estourar", BuiltInFunction("estourar"), True)
global_symbol_table.set("limpar", BuiltInFunction("limpar"), True)
global_symbol_table.set("tipo", BuiltInFunction("type"), True)
global_symbol_table.set("fatiar", BuiltInFunction("fatiar"), True)
global_symbol_table.set("aleatório", BuiltInFunction("aleatório"), True)
global_symbol_table.set("ler_arquivo", BuiltInFunction("ler_arquivo"), True)
global_symbol_table.set("escrever_arquivo", BuiltInFunction("escrever_arquivo"), True)
global_symbol_table.set("executar", BuiltInFunction("clicommand"), True)
global_symbol_table.set("eval", BuiltInFunction("eval"), True)
global_symbol_table.set("python", BuiltInFunction("python"), True)
global_symbol_table.set("update", BuiltInFunction("update"), True)
global_symbol_table.set("esperar", BuiltInFunction("esperar"), True)
global_symbol_table.set("bomba_nuclear_universal", BuiltInFunction("bomba_exit"), True)
global_symbol_table.set("pode_executar", BuiltInFunction("is_callable"), True)

def run(fn, text):
	
	global FILENAME
	lexer = Lexer(fn, text)
	tokens, error = lexer.make_tokens()
	if error: return None, error, None
	
	FILENAME = lexer.fn

	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error, None

	interpreter = Interpreter()
	context = Context('<program>')
	context.symbol_table = SymbolTable(global_symbol_table)
	global_symbol_table.child = context.symbol_table
	result = interpreter.visit(ast.node, context)
	return result.value, result.error, context