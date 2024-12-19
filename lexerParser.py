import sys  
import ply.lex as lex  
import ply.yacc as yacc  

tokens = [  
    'IF', 'TYPE', 'ASSIGN', 'BOOLEAN', 'STRING', 'FLOAT', 'INTEGER',  
    'THEN', 'ELSE', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'LT', 'GT',  
    'LEQ', 'GEQ', 'EQ', 'LPAREN', 'RPAREN', 'LACCO', 'RACCO',  
    'DIFFERENT', 'AND', 'PRINT', 'SCAN', 'WHILE', 'DO', 'FOR', 'IDENT',
    'SEMICOLON', 'VIRGULE', 'NOT', 'COMMENT'
]

# Regular expression rules for tokens  
t_IF = r'if'  
t_TYPE = r'\b(?:bool|string|float|int)\b'  
t_ASSIGN = r'='  
t_BOOLEAN = r'\b(?:true|false)\b'  
t_STRING = r'\"([^\\n]|(\\.))*?\"'  
t_FLOAT = r'\d+\.\d+'  
t_INTEGER = r'\d+'  
t_THEN = r'then'  
t_ELSE = r'else'  
t_PLUS = r'\+'  
t_MINUS = r'-'  
t_TIMES = r'\*'  
t_DIVIDE = r'/'  
t_LT = r'<'  
t_GT = r'>'  
t_LEQ = r'<='  
t_GEQ = r'>='  
t_EQ = r'=='  
t_LACCO = r'{'  
t_RACCO = r'}'  
t_DIFFERENT = r'!='  
t_AND = r'&&'  
t_LPAREN = r'\('  
t_RPAREN = r'\)'  
t_NOT = r'!'  
t_SEMICOLON = r';'  
t_VIRGULE = r','  
t_COMMENT = r'\/\/.*'
t_PRINT = r'print'  
t_SCAN = r'scan'  
t_WHILE = r'while'  
t_DO = r'do'  
t_FOR = r'for'  
t_IDENT = r'(?!if|else|while|do|for|then|int|float|string|print|scan)[a-zA-Z_][a-zA-Z0-9_]*'  


t_ignore = ' \t'  

# Track line numbers and positions  
lexer_line_start = 1  
lexer_pos_in_line = 1  

def t_newline(t):  
    r'\n+'  
    t.lexer.lineno += len(t.value)  
    global lexer_line_start, lexer_pos_in_line  
    lexer_line_start = t.lexer.lineno  
    lexer_pos_in_line = 1  

def t_error(t):  
    global lexer_pos_in_line  
    print(" ")  
    print(f"erreur lexicale: lexeme non reconnu par l'analyseur lexical '{t.value[0]}' at line {t.lineno}, pos {lexer_pos_in_line}")  
    print(" ")  
    t.lexer.skip(1)  
    sys.exit(1)  

# Define the grammar rules with actions

def p_bloc(p):  
    '''Bloc : Bloc_Instruction  
            | DeclarationList Bloc_Instruction'''  
    if len(p) == 3:  
        p[0] = (p[1], p[2])  
    else:  
        p[0] = p[1]


def p_declarationList(p):  
    '''DeclarationList : Declaration SEMICOLON DeclarationList  
                       | Declaration SEMICOLON'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    else:  
        p[0] = (p[1], p[2])  


def p_declaration(p):  
    '''Declaration : TYPE IDENT VirguleIdentList Affectation1  
                   | TYPE IDENT VirguleIdent  
                   | TYPE IDENT Affectation1  
                   | TYPE IDENT'''  
    if len(p) == 5:  
        p[0] = (p[1], p[2], p[3], p[4])  
    elif len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    else:  
        p[0] = (p[1], p[2])  


def p_affectation1(p):  
    '''Affectation1 : ASSIGN BOOLEAN  
                    | ASSIGN STRING  
                    | ASSIGN FLOAT  
                    | ASSIGN INTEGER'''  
    p[0] = (p[1], p[2])  


def p_bloc_instruction(p):  
    '''Bloc_Instruction : Instruction1 SEMICOLON Bloc_Instruction  
                        | Instruction2 Bloc_Instruction  
                        | Instruction1 SEMICOLON  
                        | Instruction2  
                        | Empty'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    elif len(p) == 3:  
        p[0] = (p[1], p[2])  
    else:  
        p[0] = p[1]  


def p_instruction1(p):  
    '''Instruction1 : Lecture  
                    | Ecriture  
                    | Affectation2'''  
    p[0] = p[1]  


def p_instruction2(p):  
    '''Instruction2 : Instruction_Conditionnelle
                     | Instruction_Repetition_While  
                     | Instruction_Repetition_For  
                     | DeclarationList  
                     | COMMENT'''  
    p[0] = p[1]
    

def p_instruction_repetition_while(p):  
    '''Instruction_Repetition_While : WHILE LPAREN Expression RPAREN DO LACCO Bloc_Instruction RACCO'''  
    if len(p) == 9:
       p[0] = (p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8])


def p_instruction_repetition_for(p):
    '''Instruction_Repetition_For : FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT LT Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT GT Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT LEQ Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT GEQ Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT DIFFERENT Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT EQ Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT LT Expression_Simple SEMICOLON Affectation2 RPAREN  Bloc_Instruction 
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT GT Expression_Simple SEMICOLON Affectation2 RPAREN  Bloc_Instruction 
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT LEQ Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction 
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT GEQ Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction 
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT DIFFERENT Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction
                                  | FOR LPAREN TYPE IDENT Affectation1 SEMICOLON IDENT EQ Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT LT Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT GT Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT LEQ Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT GEQ Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT DIFFERENT Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT EQ Expression_Simple SEMICOLON Affectation2 RPAREN LACCO Bloc_Instruction RACCO
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT LT Expression_Simple SEMICOLON Affectation2 RPAREN  Bloc_Instruction 
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT GT Expression_Simple SEMICOLON Affectation2 RPAREN  Bloc_Instruction 
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT LEQ Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction 
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT GEQ Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction 
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT DIFFERENT Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction
                                  | FOR LPAREN IDENT Affectation1 SEMICOLON IDENT EQ Expression_Simple SEMICOLON Affectation2 RPAREN Bloc_Instruction'''
    if len(p) == 15:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14])
    elif len(p) == 14:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13])
    elif len(p) == 13:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12])
    else:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11])



def p_instruction_conditionnelle(p):  
    '''Instruction_Conditionnelle : IF LPAREN Expression RPAREN THEN LACCO Bloc_Instruction RACCO ELSE LACCO Bloc_Instruction RACCO
                                  | IF LPAREN Expression RPAREN THEN LACCO Bloc_Instruction RACCO ELSE Bloc_Instruction
                                  | IF LPAREN Expression RPAREN THEN Bloc_Instruction ELSE LACCO Bloc_Instruction RACCO
                                  | IF LPAREN Expression RPAREN THEN Bloc_Instruction ELSE Bloc_Instruction
                                  | IF LPAREN Expression RPAREN THEN LACCO Bloc_Instruction RACCO
                                  | IF LPAREN Expression RPAREN THEN Bloc_Instruction'''
                                     
    if len(p) == 13:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12])
    elif len(p) == 11:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10])
    elif len(p) == 9:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6],p[7],p[8])
    elif len(p) == 7:
        p[0] = (p[1], p[2], p[3],p[4],p[5],p[6])


def p_affectation2(p):  
    '''Affectation2 : IDENT ASSIGN Expression'''  
    p[0] = (p[1], p[2], p[3])  


def p_lecture(p):  
    '''Lecture : PRINT LPAREN Expression RPAREN'''  
    p[0] = (p[1], p[2], p[3], p[4])  


def p_ecriture(p):  
    '''Ecriture : SCAN LPAREN Valeur RPAREN'''  
    p[0] = (p[1], p[2], p[3], p[4])  


def p_valeur(p):  
    '''Valeur : Entree EntreeList  
              | Entree'''  
    if len(p) == 3:  
        p[0] = (p[1], p[2])  
    else:  
        p[0] = p[1]  


def p_entreeList(p):  
    '''EntreeList : VIRGULE Entree EntreeList  
                  | VIRGULE Entree'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    else:  
        p[0] = (p[1], p[2])  


def p_entree(p):  
    '''Entree : STRING  
              | BOOLEAN  
              | FLOAT  
              | INTEGER'''  
    p[0] = p[1]  


def p_expression(p):  
    '''Expression : Expression_Simple OperateurComparaison Expression_Simple  
                  | Expression_Simple'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    else:  
        p[0] = p[1]


def p_operateur_comparaison(p):  
    '''OperateurComparaison : LT  
                            | GT  
                            | LEQ  
                            | GEQ  
                            | EQ  
                            | DIFFERENT  
                            | AND'''  
    p[0] = p[1]  


def p_expression_simple(p):  
    '''Expression_Simple : signe Terme OperateurAddSousTermeList  
                         | signe Terme  
                         | Terme OperateurAddSousTermeList  
                         | Terme'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    elif len(p) == 3:  
        p[0] = (p[1], p[2])  
    else:  
        p[0] = p[1]  


def p_signe(p):  
    '''signe : PLUS  
             | MINUS'''  
    p[0] = p[1]  

def p_operateur_add_sous_terme_list(p):
    '''OperateurAddSousTermeList : OperateurAddSous Terme OperateurAddSousTermeList
                                 | OperateurAddSous Terme'''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = (p[1], p[2])
        

def p_operateur_add_sous(p):  
    '''OperateurAddSous : PLUS  
                        | MINUS'''  
    p[0] = p[1]  


def p_terme(p):  
    '''Terme : Facteur OperateurMulDivFacteurList  
             | Facteur'''  
    if len(p) == 3:  
        p[0] = (p[1], p[2])  
    else:  
        p[0] = p[1] 

def p_operateurMulDivFacteurList(p):  
    '''OperateurMulDivFacteurList : OperateurMulDiv Facteur OperateurMulDivFacteurList  
                                  | OperateurMulDiv Facteur'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    else:  
        p[0] = (p[1], p[2])  

def p_operateur_mul_div(p):  
    '''OperateurMulDiv : TIMES  
                       | DIVIDE'''  
    p[0] = p[1]  
 
def p_facteur(p):  
    '''Facteur : LPAREN Expression RPAREN  
               | NOT Facteur  
               | IDENT  
               | BOOLEAN  
               | INTEGER  
               | FLOAT  
               | STRING'''  
    if len(p) == 4:  
        p[0] = (p[1], p[2], p[3])  
    elif len(p) == 3:  
        p[0] = (p[1], p[2])  
    else:  
        p[0] = p[1]


def p_virgule_ident_list(p):
    '''VirguleIdentList : VIRGULE IDENT VirguleIdentList
                        | VIRGULE IDENT'''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = (p[1], p[2])

def p_virgule_ident(p):
    '''VirguleIdent : VIRGULE IDENT'''
    p[0] = (p[1], p[2])

    

def p_empty(p):  
    'Empty :'  
    pass  


# Error rule for syntax errors  
def p_error(p):  
    if p:  
        print(f"Syntax error at line {p.lineno}, pos {p.lexpos}: Unexpected token '{p.value}' in rule '{p.type}'")  
        print(" ")  
        print("La sequence d'instructions est syntaxiquement incorrecte")  
        print("Les instructions ne sont pas reconnus par le PARSER")  
        print(" ")  
        sys.exit(1)  

    else:  
        print("Syntax error: Unexpected end of input")  
        print(" ")  
        print("La sequence d'instructions est syntaxiquement incorrecte")  
        print("Les instructions ne sont pas reconnus par le PARSER")  
        print(" ")  
        sys.exit(1)  



# Build the lexer  
lexer = lex.lex()

# Read the contents of the file
with open('input.txt', 'r') as file:
    data = file.read()  
   
print("")  
print("")  
print("LEXER")  
print("")  
lexer.input(data)  

#lexer_pos_in_line = 0  # Initialisation de lexer_pos_in_line

while True:  
    tok = lexer.token()  
    if not tok:  
        break  # No more input  
    tok.lexpos = lexer_pos_in_line  
    lexer_pos_in_line += len(tok.value)  
    print(f"{tok.type} at line {tok.lineno}, pos {tok.lexpos}: {tok.value}")  
    print("La sequence d'instructions est lexicalement correcte")
    

print("PARSER")  
print("")  
print("")  

# Build the parser  
parser = yacc.yacc()  
result = parser.parse(data,lexer=lexer)  
print(result)  

print("")  
print("")  
print("La sequence d'instructions est syntaxiquement correcte")  
print("Tous les instructions sont reconnus par le PARSER")  
print("")  
print("")