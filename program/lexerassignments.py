#Class to wrap the different tokens we'll be using
from enum import Enum
class TokenType(Enum):
    identifier = 1
    integer = 2
    whitespace = 3
    equals = 4
    semicolon = 5
    
    typeExpr = 6 # done
    booleanLiteral = 7
##    integerLiteral = 8
    floatLiteral = 9
    colourLiteral = 10 # done
    padWidth = 11
    padHeight = 12
    padReadKeyword = 13
    padRandIKeyword = 14
    
    multiplicativeOp = 15
    additiveOp = 16
    relationalOp = 17
    
    # brackets - done
    openBracket = 18
    closeBracket = 19
    openCurlyBracket = 20
    closeCurlyBracket = 21
    openSquareBracket = 22
    closeSquareBracket = 23
    
    colon = 24

    letKeyword = 25 # done
    printKeyword = 26
    delayKeyword = 27
    writeBoxKeyword = 28
    writeKeyword = 29
    returnKeyword = 30
    ifKeyword = 31
    elseKeyword = 32
    forKeyword = 33
    whileKeyword = 34
    funKeyword = 35 # function
    notKeyword = 36 # assume that no need to take care of '-' in unary since addOp '-' has much similar meaning
    asKeyword = 37
    
    minusSign = 38
    arrow = 39
    
    # special tokens
    void = 99
    end = 100 # not used

class Token:
    def __init__(self, t, l):
        self.type = t
        self.lexeme = l        

#Lexer class wrapper for code above
class Lexer:
    def __init__(self):
        self.lexeme_list = ["_", "letter", "digit", "ws", "eq", "sc", "colon", "comma", '(',')','{','}','[',']', '#','hexLetter',
                            '.', 'multiOp','addOp','relOp','!','-','>', "other"] # alphabet
        # , "oBrac", "cBrac", "ocBrac", "ccBrac", "osBrac", "csBrac"
        self.states_list = [x for x in range(50)]
        self.states_accp = [1, 2, 3, 4, 5] + list(range(6,14)) + [20] + list(range(22,26)) + [27,28]+[31,32]

        self.rows = len(self.states_list)
        self.cols = len(self.lexeme_list)

        # Let's take integer -1 to represent the error state for this DFA
        self.Tx = [[-1 for j in range(self.cols)] for i in range(self.rows)]
        self.InitialiseTxTable();     

    def InitialiseTxTable(self):
        # Update Tx to represent the state transition function of the DFA
        # Variables - do not start with _ or digit
        self.Tx[0][self.lexeme_list.index("letter")] = 1
        self.Tx[0][self.lexeme_list.index("hexLetter")] = 1 # a-f are not part of 'letter' category
        # do not accept underscore in an identifier # self.Tx[0][self.lexeme_list.index("_")] = 1
        self.Tx[1][self.lexeme_list.index("letter")] = 1 # g-z
        self.Tx[1][self.lexeme_list.index("hexLetter")] = 1 # a-f
        self.Tx[1][self.lexeme_list.index("digit")] = 1 # a2
        self.Tx[1][self.lexeme_list.index("_")] = 1 # a_

        #White Space
        self.Tx[0][self.lexeme_list.index("ws")] = 2
        self.Tx[2][self.lexeme_list.index("ws")] = 2

        #Eq sign (=)
        self.Tx[0][self.lexeme_list.index("eq")] = 3        

        #Integers
        self.Tx[0][self.lexeme_list.index("digit")] = 4        
        self.Tx[4][self.lexeme_list.index("digit")] = 4

        #Semicolon sign (;)
        self.Tx[0][self.lexeme_list.index("sc")] = 5

        
        # Colon sign (:)
        self.Tx[0][self.lexeme_list.index("colon")] = 6

        # Comma sign (,)
        self.Tx[0][self.lexeme_list.index("comma")] = 7

        # Brackets
        self.Tx[0][self.lexeme_list.index("(")] = 8
        self.Tx[0][self.lexeme_list.index(")")] = 9
        self.Tx[0][self.lexeme_list.index("{")] = 10
        self.Tx[0][self.lexeme_list.index("}")] = 11
        self.Tx[0][self.lexeme_list.index("[")] = 12
        self.Tx[0][self.lexeme_list.index("]")] = 13
        
        # Hexadecimal literal
        self.Tx[0][self.lexeme_list.index("#")] = 14
        self.Tx[14][self.lexeme_list.index("hexLetter")] = 15
        self.Tx[15][self.lexeme_list.index("hexLetter")] = 16
        self.Tx[16][self.lexeme_list.index("hexLetter")] = 17
        self.Tx[17][self.lexeme_list.index("hexLetter")] = 18
        self.Tx[18][self.lexeme_list.index("hexLetter")] = 19
        self.Tx[19][self.lexeme_list.index("hexLetter")] = 20
        self.Tx[14][self.lexeme_list.index("digit")] = 15
        self.Tx[15][self.lexeme_list.index("digit")] = 16
        self.Tx[16][self.lexeme_list.index("digit")] = 17
        self.Tx[17][self.lexeme_list.index("digit")] = 18
        self.Tx[18][self.lexeme_list.index("digit")] = 19
        self.Tx[19][self.lexeme_list.index("digit")] = 20
        
        #float
        self.Tx[4][self.lexeme_list.index(".")] = 21
        self.Tx[21][self.lexeme_list.index("digit")] = 22
        self.Tx[22][self.lexeme_list.index("digit")] = 22

        # operators
        self.Tx[0][self.lexeme_list.index("multiOp")] = 23 # 'and' and 'or' are taken care of by keywordLookup()
        self.Tx[0][self.lexeme_list.index("addOp")] = 24

        self.Tx[3][self.lexeme_list.index("eq")] = 25 # ==  # update code elsewhere
        self.Tx[0][self.lexeme_list.index("!")] = 26
        self.Tx[26][self.lexeme_list.index("eq")] = 25 # !=
        self.Tx[0][self.lexeme_list.index("relOp")] = 27 # < >
        self.Tx[27][self.lexeme_list.index("eq")] = 25 # <= >=

        self.Tx[0][self.lexeme_list.index("-")] = 28


        # __ built-in functions
        self.Tx[0][self.lexeme_list.index("_")] = 29
        self.Tx[29][self.lexeme_list.index("_")] = 30
        self.Tx[30][self.lexeme_list.index("letter")] = 31
        self.Tx[30][self.lexeme_list.index("hexLetter")] = 31
        self.Tx[31][self.lexeme_list.index("letter")] = 31
        self.Tx[31][self.lexeme_list.index("hexLetter")] = 31
        self.Tx[31][self.lexeme_list.index("_")] = 31 # __write_box
        
        # arrow
        self.Tx[28][self.lexeme_list.index(">")] = 32
        

        
        # keywords are taken care of by keywordLookup() function, after they are first recognised as identifiers.
        
        if debug == True:
            for row in self.Tx: # displays DFSA transitions table
                print(row)

    def AcceptingStates(self, state):
        try:
            self.states_accp.index(state)
            return True;
        except ValueError:
            return False;

    def keywordLookup(self, lexeme): # some words may be keywords instead of identifiers
        # assumes that eg '__a' is a valid identifier
        
        if (lexeme == "float") | (lexeme == "int") | (lexeme == "bool") | (lexeme == "colour"): return Token(TokenType.typeExpr, lexeme)
        elif (lexeme == "true") | (lexeme == "false"): return Token(TokenType.booleanLiteral, lexeme)

        
        elif lexeme == 'for': return Token(TokenType.forKeyword, lexeme)
        elif lexeme == 'while': return Token(TokenType.whileKeyword, lexeme)
        elif lexeme == 'if': return Token(TokenType.ifKeyword, lexeme)
        elif lexeme == 'else': return Token(TokenType.elseKeyword, lexeme)
        elif lexeme == 'fun': return Token(TokenType.funKeyword, lexeme)
        elif lexeme == 'let': return Token(TokenType.letKeyword, lexeme)
        elif lexeme == 'return': return Token(TokenType.returnKeyword, lexeme)

        elif lexeme == 'and': return Token(TokenType.multiplicativeOp, lexeme)
        elif lexeme == 'or': return Token(TokenType.additiveOp, lexeme)
        elif lexeme == 'not': return Token(TokenType.notKeyword, lexeme)

        elif lexeme == 'as': return Token(TokenType.asKeyword, lexeme)
        
    def builtInFuncLookup(self, lexeme): # similar to keywordLookup, but start with '__'
        if lexeme == '__width': return Token(TokenType.padWidth, lexeme)
        elif lexeme == '__height': return Token(TokenType.padHeight, lexeme)
        elif lexeme == '__read': return Token(TokenType.padReadKeyword, lexeme)
        elif lexeme == '__random_int': return Token(TokenType.padRandIKeyword, lexeme)

        elif lexeme == '__print': return Token(TokenType.printKeyword, lexeme)
        elif lexeme == '__delay': return Token(TokenType.delayKeyword, lexeme)
        elif lexeme == '__write_box': return Token(TokenType.writeBoxKeyword, lexeme)
        elif lexeme == '__write': return Token(TokenType.writeKeyword, lexeme)
        
        
    def GetTokenTypeByFinalState(self, state, lexeme):
        if state == 1:
            keywordToken = self.keywordLookup(lexeme)
            if keywordToken is not None: return keywordToken
            else: return Token(TokenType.identifier, lexeme)
        elif state == 2:
            return Token(TokenType.whitespace, lexeme)
        elif state == 3:
            return Token(TokenType.equals, lexeme)
        elif state == 4:
            return Token(TokenType.integer, lexeme)
        elif state == 5:
            return Token(TokenType.semicolon, lexeme)

        elif state == 6:
            return Token(TokenType.colon, lexeme)
        elif state == 7:
            return Token(TokenType.comma, lexeme)
        
        elif state == 8: return Token(TokenType.openBracket, lexeme)
        elif state == 9: return Token(TokenType.closeBracket, lexeme)
        elif state == 10: return Token(TokenType.openCurlyBracket, lexeme)
        elif state == 11: return Token(TokenType.closeCurlyBracket, lexeme)
        elif state == 12: return Token(TokenType.openSquareBracket, lexeme)
        elif state == 13: return Token(TokenType.closeSquareBracket, lexeme)
        
        elif state == 20: return Token(TokenType.colourLiteral, lexeme) # hex literal
        elif state == 22: return Token(TokenType.floatLiteral, lexeme)

        elif state == 23: return Token(TokenType.multiplicativeOp, lexeme)
        elif state == 24: return Token(TokenType.additiveOp, lexeme)
        elif (state == 25) | (state == 27): return Token(TokenType.relationalOp, lexeme)
        elif state == 28: return Token(TokenType.minusSign, lexeme)
        elif state == 31:
            keywordToken = self.builtInFuncLookup(lexeme)
            if keywordToken is not None: return keywordToken
            else: return Token(TokenType.void, "error")
        elif state == 32: return Token(TokenType.arrow, lexeme)
        
        else:
            return 'default result'


    def CatChar(self, character):
        cat = "other"
        if character.isalpha(): cat = "letter"
        elif character.isdigit(): cat = "digit"
        elif character == "_": cat = "_"
        elif character == " ": cat = "ws"
        elif character == ";": cat = "sc"
        elif character == "=": cat = "eq"
        elif character == ":": cat = "colon"
        elif character == ",": cat = "comma"
        elif character in '(){}[]#.!->': return character # '(',')','{','}','[',']'
        elif character in '*/': cat = 'multiOp' # 'and' and 'or' are taken care of by keywordLookup()
        elif character in '+': cat = 'addOp'
        elif (character == '<') | (character == '==') | (character == '!=') | (character == '<=') | (character == '>='): cat = 'relOp' # '>' excluded

        if character in 'abcdefABCDEF': cat = "hexLetter"
        
        return cat

    def EndOfInput(self, src_program_str, src_program_idx):
        if (src_program_idx > len(src_program_str)-1):
            return True;
        else:
            return False;

    def NextChar(self, src_program_str, src_program_idx):
        if (not self.EndOfInput(src_program_str, src_program_idx)):
            return True, src_program_str[src_program_idx]
        else: 
            return False, "."

    def NextToken(self, src_program_str, src_program_idx):
        state = 0  #initial state is 0 - check Tx
        stack = []
        lexeme = ""
        stack.append(-2);  #insert the error state at the bottom of the stack.
        
        while (state != -1):
            if self.AcceptingStates(state): 
                stack.clear();
            stack.append(state);
            
            exists, character = self.NextChar(src_program_str, src_program_idx);
            lexeme += character
            if (not exists): 
                #print("LAST LEXEME: ", lexeme); 
                break  #Break out of loop if we're at the end of the string
            src_program_idx = src_program_idx + 1
            
            cat = self.CatChar(character);
            state = self.Tx[state][self.lexeme_list.index(cat)];
            #print("Lexeme: ", lexeme, " => NEXT STATE: ", state, "  => CAT: ", cat, "  => CHAR:", character, "  => STACK: ", stack)
        
        lexeme = lexeme[:-1] #remove the last character added which sent the lexer to state -1    

        syntax_error = False;
        #rollback
        while (len(stack) > 0):
            if (stack[-1] == -2): #report a syntax error
                syntax_error = True
                break    

            #Pop this state if not an accepting state.
            if (not self.AcceptingStates(stack[-1])):
                stack.pop();
                print("POPPED => ", stack)
                lexeme = lexeme[:-1]
            
            #This is an accepting state ... return it.    
            else:
                state = stack.pop()
                break
        
        #print("Lexeme: ", lexeme, "with state: ", state)

        if syntax_error:
            return Token(TokenType.void, "error"), "error"

        if self.AcceptingStates(state):
            return self.GetTokenTypeByFinalState(state, lexeme), lexeme
        else: 
            return Token(TokenType.void, "error"), "error"


    def GenerateTokens(self, src_program_str):
        print("INPUT:: " + src_program_str)
        tokens_list = []
        src_program_idx = 0;
        token, lexeme = self.NextToken(src_program_str, src_program_idx)
        tokens_list.append(token);

        while (token != -1):
            src_program_idx = src_program_idx + len(lexeme)
            #print ("Nxt TOKEN: ", token, " ", lexeme, "(", len(lexeme), ")  => IDX: ", src_program_idx)
            if (not self.EndOfInput(src_program_str, src_program_idx)):
                token, lexeme = self.NextToken(src_program_str, src_program_idx)
                tokens_list.append(token);
            else: break

        #print("DONE!!")
        return tokens_list;


def read_file_as_string(file_path):
    try:
        with open(file_path, 'r') as file:
            file_content = file.read()
        return file_content
    except FileNotFoundError:
        print(f"File '{file_path}' not found.")
        return None
    except Exception as e:
        print(f"Error reading file: {e}")
        return None

debug = False
if __name__ == '__main__':
##    strIn = "_ __ __h __wdth __write_box __print not nota-b a_b - <= > != <= > != a b"
    strIn = read_file_as_string("code.parl")
    if strIn is None: exit() # user has been notified of error already
    strIn = strIn.replace("\n", " ")
    
    lex = Lexer()
##    testString = "x=23 ; y= 100;   z=4.0 ; [#98ffFf]jg{g} [f fd] for:while __width __write_box hello"
    toks = lex.GenerateTokens(strIn)

    for t in toks:
        print(t.type, t.lexeme)
