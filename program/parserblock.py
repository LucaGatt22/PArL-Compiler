# Now we need the parser (using tokens produced by the Lexer) to build the AST
# A predictive recursive descent parser
from printnodesvisitor import PrintNodesVisitor
import lexerassignments as lex
import astnodes as ast

class Parser:
    def __init__(self, src_program_str):
        self.name = "PARSEAR"
        self.lexer = lex.Lexer()
        self.index = -1  #start at -1 so that the first token is at index 0
        self.src_program = src_program_str
        self.tokens = self.lexer.GenerateTokens(self.src_program) # token is pair of tokenType and lexeme
        #print("[Parser] Lexer generated token list ::")
        #for t in self.tokens:
        #    print(t.type, t.lexeme)
        self.crtToken = lex.Token("", lex.TokenType.void) # current token
        self.nextToken = lex.Token("", lex.TokenType.void)
        # self.ASTroot = ast.ASTAssignmentNode     #this will need to change once you introduce the AST program node .... that should become the new root node

        self.test = False # test/debug
        self.failIterCount = 0
        
    def NextTokenSkipWS(self):
        self.index += 1   #Grab the next token
        if (self.index < len(self.tokens)):
            self.crtToken = self.tokens[self.index]
        else:
            self.crtToken = lex.Token(lex.TokenType.end, "END")

    def NextToken(self):
        self.NextTokenSkipWS()
        while (self.crtToken.type == lex.TokenType.whitespace):
            #print("--> Skipping WS")
            self.NextTokenSkipWS()

        #print("Next Token Set to ::: ", self.crtToken.type, self.crtToken.lexeme)                 

    def PrevTokenSkipWS(self):
        self.index -= 1   #Grab the next token
        if (self.index >= 0):
            self.crtToken = self.tokens[self.index]
        else:
            print('FATAL ERROR')

    def PrevToken(self):
        self.PrevTokenSkipWS()
        while (self.crtToken.type == lex.TokenType.whitespace):
            #print("--> Skipping WS")
            self.PrevTokenSkipWS()
        
    def checkFailIterCount(self):
        self.failIterCount += 1
        if self.failIterCount == 10:
            print('infinite loop')
            exit()


    '''
##    calling the function ParseToken()
##    ParseToken(lex.TokenType.typeExpr, ast.ASTTypeNode( lexeme ) )
##    This does not work since must have lexeme when it is assigned in fucntion itself.
##    So function discarded.
    def ParseToken(self, tokenType, node):
        if (self.crtToken.type == tokenType):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTBoolNode( lexeme ) # return node of token parsed
            # do not write self.crtToken.lexeme in `return` statement becuase of the self.NextToken() function
    '''
    def ParseType(self):
        if (self.crtToken.type == lex.TokenType.typeExpr):
            typeExpr:str = self.crtToken.lexeme
            typeNode = ast.ASTTypeNode( typeExpr )
            self.NextToken()
            return typeNode
        
    def ParseBool(self):
        if (self.crtToken.type == lex.TokenType.booleanLiteral):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTBoolNode( lexeme )

    ParseInteger = lambda self: self.ParseInt() # alias
    def ParseInt(self):
        if (self.crtToken.type == lex.TokenType.integer):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTIntegerNode( lexeme )

    def ParseFloat(self):
        if (self.crtToken.type == lex.TokenType.floatLiteral):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTFloatNode( lexeme )

    def ParseColour(self):
        if (self.crtToken.type == lex.TokenType.colourLiteral):
            lexeme:str = self.crtToken.lexeme
            lexeme = lexeme[1:] # removed '#' from colour
            self.NextToken()
            return ast.ASTColourNode( lexeme )

    def ParseTokenNoValue(self, tokenType):
        if (self.crtToken.type == tokenType):
            self.NextToken()
            return True
        return False
    def ParsePadWidth(self):
        if self.ParseTokenNoValue(lex.TokenType.padWidth):
            return ast.ASTPadWidthNode()
    def ParsePadHeight(self):
        if self.ParseTokenNoValue(lex.TokenType.padHeight):
            return ast.ASTPadHeightNode()

    def ParsePadRead(self):
        if self.ParseTokenNoValue(lex.TokenType.padReadKeyword):
            expr1 = self.ParseExpr()
            if (expr1 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                expr2 = self.ParseExpr()
                if expr2 != None: return ast.ASTPadReadNode(expr1, expr2)

    def ParsePadRandI(self):
        if self.ParseTokenNoValue(lex.TokenType.padRandIKeyword):
            expr = self.ParseExpr()
            if expr != None: return ast.ASTPadRandINode( expr )
    
    def ParseAnOption(self, parseFuncs):
        if self.test: print('accessed ParseAnOption')
        for parseFunc in parseFuncs:
##            if self.test: print('accessed parseFunc in ParseAnOption')
            if self.test:
                if (parseFunc == self.ParseIdentifier) & (self.ParseFunctionCall in parseFuncs): print('true')
##                
##                print(f'"parseFunc == self.ParseIdentifier"={parseFunc == self.ParseIdentifier} "self.ParseFunctionCall in parseFuncs"={self.ParseFunctionCall in parseFuncs}')
            if (parseFunc == self.ParseIdentifier) & (self.ParseFunctionCall in parseFuncs):
                if self.test:
                    print('found ParseIdentifier in ParseAnOption')
                    print(f'parseFunc={parseFunc}')
##                    print(self.tokens[self.index+1])
                    print((self.index+1 < len(self.tokens)))
                if (self.crtToken.type == lex.TokenType.identifier) & (self.index+1 < len(self.tokens)):
                    if (self.tokens[self.index+1].type == lex.TokenType.openBracket):
                        node = self.ParseFunctionCall()
                elif self.crtToken.type == lex.TokenType.identifier: node = self.ParseIdentifier()
            elif (parseFunc == self.ParseFunctionCall) & (self.ParseIdentifier in parseFuncs):
                pass
            else: node = parseFunc()
            if node != None:
                if self.test: print(f'return node in ParseAnOption: {node}')
                return node
    ParseLiteral = lambda self: self.ParseAnOption([
        self.ParseBool, self.ParseInt, self.ParseFloat,
        self.ParseColour, self.ParsePadWidth,
        self.ParsePadHeight, self.ParsePadRead ])

    def ParseIdentifier(self):
        if self.test: print('accessed ParseIdentifier')
        
        if (self.crtToken.type == lex.TokenType.identifier):
            lexemeIdentifier:str = self.crtToken.lexeme
            identifierNode = ast.ASTVariableNode( lexemeIdentifier )
            self.NextToken()
            
            # check for array part
            if self.ParseTokenNoValue(lex.TokenType.openSquareBracket):
                expr = self.ParseExpr()
                if (expr != None) & self.ParseTokenNoValue(lex.TokenType.closeSquareBracket):
                    return ast.ASTIdentifierArrayNode( identifierNode, expr )
            else: return identifierNode

    def ParseMultiplicativeOp(self):
        #multiop
        if (self.crtToken.type == lex.TokenType.multiplicativeOp):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTMultiOpNode( lexeme )
        
    def ParseAdditiveOp(self):
        # addop and minus
        if (self.crtToken.type == lex.TokenType.additiveOp) | (self.crtToken.type == lex.TokenType.minusSign):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTAddOpNode( lexeme )

    def ParseRelationalOp(self):
        if (self.crtToken.type == lex.TokenType.relationalOp):
            lexeme:str = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTRelOpNode( lexeme )

    def ParseActualParams(self):
        expr = self.ParseExpr()
        if expr != None:
            exprs = [ expr ]
            while self.ParseTokenNoValue( lex.TokenType.comma ):
                expr = self.ParseExpr()
                if expr != None: exprs.append( expr )

    
    def ParseFunctionCall(self):
        if self.test:
            print('accessed ParseFunctionCall()')
        identifier = self.ParseIdentifier()
        if identifier != None:
            if self.ParseTokenNoValue( lex.TokenType.openBracket ):
                actualParams = self.ParseActualParams()
                if self.ParseTokenNoValue( lex.TokenType.closeBracket ):
                    return ast.ASTFunctionCallNode(identifier, actualParams)
            else: ast.ASTFunctionCallNode(identifier)

    def ParseSubExpr(self):
        if self.ParseTokenNoValue( lex.TokenType.openBracket ):
            expr = self.ParseExpr()
            if (expr != None) & self.ParseTokenNoValue( lex.TokenType.closeBracket ):
                return ast.ASTSubExprNode(expr)

    def ParseUnary(self):
        # '-' and 'not' may be different
        if (self.crtToken.type == lex.TokenType.minusSign) | (self.crtToken.type == lex.TokenType.notKeyword):
            unaryOp = self.crtToken
            self.NextToken()

            expr = self.ParseExpr()
            if expr != None: return ast.ASTUnaryNode(unaryOp, expr)
            
    ParseFactor = lambda self: self.ParseAnOption([
        self.ParseLiteral, self.ParseIdentifier, self.ParseFunctionCall, self.ParseSubExpr,
        self.ParseUnary, self.ParsePadRandI #, self.ParsePadWidth,
        #self.ParsePadHeight, self.ParsePadRead
    ])

    def ParseTerm(self):
        if self.test: print('accessed ParseTerm()')
        factor = self.ParseFactor()
        if factor != None:
            term = ast.ASTTermNode( factor )
            while True:
                multiplicativeOp = self.ParseMultiplicativeOp()
                if multiplicativeOp == None: return term
                factor = self.ParseFactor()
                if factor == None:
                    print('error while parsing term')
                    return None
                term.add_factor(multiplicativeOp, factor)
        
    def ParseSimpleExpr(self):
        if self.test: print('accessed ParseSimpleExpr()')
        term = self.ParseTerm()
        if term != None:
            simpleExprNode = ast.ASTSimpleExprNode( term )
            while True:
                relOp = self.ParseAdditiveOp()
                if relOp == None: return simpleExprNode # does this work? - continue from here
                term = self.ParseTerm()
                if term == None:
                    print('error while parsing term')
                    return None
                simpleExprNode.add_term(relOp, term)
        

    def oldFuncParseExpression(self): # to remove
        #for now we'll assume an expression can only be an integer
        if (self.crtToken.type == lex.TokenType.integer):
            value = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTIntegerNode(value)
        if self.crtToken.type == lex.TokenType.floatLiteral:
            value = self.crtToken.lexeme
            self.NextToken()
            if self.crtToken.type == lex.TokenType.floatLiteral: pass # continue from here

    ParseExpr = lambda self: self.ParseExpression() # alias
    def ParseExpression(self):
        if self.test: print('accessed ParseExpression()')
        simpleExpr = self.ParseSimpleExpr()
        if simpleExpr != None:
            exprNode = ast.ASTExprNode( simpleExpr )
            while True:
                relOp = self.ParseRelationalOp()
                if relOp == None: break # does this work? - continue from here
                simpleExpr = self.ParseSimpleExpr()
                if simpleExpr == None:
                    print('error ParseExpression')
                    return None
                exprNode.add_simpleexpr(relOp, simpleExpr)
            # check for 'as' keyword
            if self.ParseTokenNoValue(lex.TokenType.asKeyword):
                typeNode = self.ParseType()
                if typeNode != None:
                    exprNode.add_type( typeNode )
                    return exprNode
                else:
                    print("error while parsing expression")
                    return None
            else: return exprNode
        
    def ParseAssignment(self):
        if self.test:
            print('accessed ParseAssignment()')
            print(self.crtToken.type)
            if self.crtToken.type == lex.TokenType.openBracket:
                print('openBracket error in ParseAssignment()')
                exit()
        
        #Assignment is made up of two main parts; the LHS (the variable) and RHS (the expression)
        if (self.crtToken.type == lex.TokenType.identifier):
            if self.test: print('accessed identifier in ParseAssignment()')
            #create AST node to store the identifier            
            assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.NextToken()
            #print("Variable Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)

            if (self.crtToken.type == lex.TokenType.equals):
                #no need to do anything ... token can be discarded
                self.NextToken()
                #print("EQ Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)

                #Next sequence of tokens should make up an expression ... therefore call ParseExpression that will return the subtree representing that expression
                assignment_rhs = self.ParseExpression()
                
                if assignment_rhs != None: return ast.ASTAssignmentNode(assignment_lhs, assignment_rhs)

    def ParseVariableDeclArray(self):
##        if (self.crtToken.type == lex.TokenType.integer):
##            integer = ast.ASTIntegerNode(self.crtToken.lexeme)
        integer = ParseInt()
        if integer:
            if self.ParseTokenNoValue(lex.TokenType.closeSquareBracket):
                if self.ParseTokenNoValue(lex.TokenType.equals):
                    if self.ParseTokenNoValue(lex.TokenType.openSquareBracket):
                        literal = self.ParseLiteral()
                        if literal: return ast.ASTVariableDeclArrayNode(integer, literal)
        elif self.ParseTokenNoValue(lex.TokenType.closeSquareBracket): # ]
            if self.ParseTokenNoValue(lex.TokenType.equals): # =
                if self.ParseTokenNoValue(lex.TokenType.openSquareBracket): # [
                    node = ast.ASTVariableDeclArrayNode( ParseLiteral() )
                    while self.ParseTokenNoValue(lex.TokenType.comma):
                        literal = self.ParseLiteral()
                        if literal: node.addLiteral( literal )

    def ParseVariableDeclSuffix(self):
        if self.ParseTokenNoValue(lex.TokenType.colon):
            typeLiteral = self.ParseType()
            if (typeLiteral != None):
                if self.ParseTokenNoValue(lex.TokenType.equals):
                    expr = self.ParseExpr()
                    if expr != None: return ast.ASTVariableDeclSuffixNode(typeLiteral, expr)
                elif self.ParseTokenNoValue(lex.TokenType.openSquareBracket):
                    return ast.ASTVariableDeclSuffixNode( self.ParseVariableDeclArray() )
        # raise Exception("FATAL ERROR - Function should not reach this line.") # can write this for all functions, although an error would still occur when not written
            
    def ParseVariableDecl(self):
        if self.ParseTokenNoValue(lex.TokenType.letKeyword):
            identifier = self.ParseIdentifier()
            if identifier != None:
                variableDeclSuffix = self.ParseVariableDeclSuffix()
                if variableDeclSuffix != None: return ast.ASTVariableDeclNode( identifier, variableDeclSuffix )
    # above def ParseVariableDeclSuffix
    # above def ParseVariableDeclArray

    def ParsePrintStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.printKeyword):
            expr = self.ParseExpr()
            if expr != None: return ast.ASTPrintStatementNode( expr )
    def ParseDelayStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.delayKeyword):
            expr = self.ParseExpr()
            if expr != None: return ast.ASTDelayStatementNode( expr )

    def ParseWriteStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.writeBoxKeyword):
            expr1 = self.ParseExpr()
            if (expr1 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                expr2 = self.ParseExpr()
                if (expr2 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                    expr3 = self.ParseExpr()
                    if (expr3 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                        expr4 = self.ParseExpr()
                        if (expr4 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                            expr5 = self.ParseExpr()
                            if expr5 != None: return ast.ASTWriteStatementNode(expr1,expr2,expr3,expr4,expr5)
        elif self.ParseTokenNoValue(lex.TokenType.writeKeyword):
            expr1 = self.ParseExpr()
            if (expr1 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                expr2 = self.ParseExpr()
                if (expr2 != None) & self.ParseTokenNoValue(lex.TokenType.comma):
                    expr3 = self.ParseExpr()
                    if expr3 != None: return ast.ASTWriteStatementNode(expr1,expr2,expr3)
    def ParseRtrnStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.returnKeyword):
            expr = self.ParseExpr()
            if expr != None: return ast.ASTRtrnStatementNode( expr )

    def ParseIfStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.ifKeyword):
            if self.ParseTokenNoValue(lex.TokenType.openBracket):
                expr = self.ParseExpr()
                if (expr != None) & self.ParseTokenNoValue(lex.TokenType.closeBracket):
                    blockIf = self.ParseBlock()
                    if blockIf != None:
                        if self.ParseTokenNoValue(lex.TokenType.elseKeyword):
                            blockElse = self.ParseBlock()
                            if blockElse != None: return ast.ASTIfStatementNode(expr, blockIf, blockElse)
                        else: return ast.ASTIfStatementNode(expr, blockIf)

    def ParseForStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.ifKeyword):
            if self.ParseTokenNoValue(lex.TokenType.openBracket):
                variableDecl = self.ParseVariableDecl() # [] taken care of by None
                if variableDecl & self.ParseTokenNoValue(lex.TokenType.semicolon):
                    expr = self.ParseExpr()
                    if (expr != None) & self.ParseTokenNoValue(lex.TokenType.semicolon):
                        assignment = self.ParseAssignment()
                        if (assignment != None) & self.ParseTokenNoValue(lex.TokenType.closeBracket):
                            block = self.ParseBlock()
                            if block != None: return ast.ASTForStatementNode(expr, block, variableDecl, assignment)
    def ParseWhileStatement(self):
        if self.ParseTokenNoValue(lex.TokenType.whileKeyword):
            if self.ParseTokenNoValue(lex.TokenType.openBracket):
                expr = self.ParseExpr()
                if (expr != None) & self.ParseTokenNoValue(lex.TokenType.closeBracket):
                    block = ParseBlock()
                    if block != None: return ast.ASTWhileStatementNode(expr, block)

    def ParseFormalParam(self):
        identifier = self.ParseIdentifier()
        if (identifier != None) & self.ParseTokenNoValue(lex.TokenType.colon):
            typeLiteral = self.ParseType()
            if typeLiteral != None:
                if self.ParseTokenNoValue(lex.TokenType.openSquareBracket):
                    integer = self.ParseInteger()
                    if (integer != None) & self.ParseTokenNoValue(lex.TokenType.closeSquareBracket):
                        return ast.ASTFormalParamNode(identifier, typeLiteral, integer)
                else: return ast.ASTFormalParamNode(identifier, typeLiteral)

    def ParseFormalParams(self):
        param = self.ParseFormalParam()
        if param != None:
            while self.ParseTokenNoValue(lex.TokenType.comma):
                self.ParseFormalParam()

    def ParseFunctionDecl(self):
        if self.test: print('accessed ParseFunctionDecl')
        if self.ParseTokenNoValue(lex.TokenType.funKeyword):
            identifier = self.ParseIdentifier()
            if (identifier != None) & self.ParseTokenNoValue(lex.TokenType.openBracket):
                if self.test: print('accessed openBracket in ParseFunctionDecl')
                params = self.ParseFormalParams() # [] taken care of by None
                if self.ParseTokenNoValue(lex.TokenType.closeBracket): # params can be None
                    if self.test: print('accessed params and closeBracket in ParseFunctionDecl')
                    if self.ParseTokenNoValue(lex.TokenType.arrow):
                        if self.test: print('accessed arrow in ParseFunctionDecl')
                        typeLiteral = self.ParseType()
                        if typeLiteral != None:
                            if self.test: print('accessed type in ParseFunctionDecl')
                            if self.ParseTokenNoValue(lex.TokenType.openSquareBracket):
                                integer = self.ParseInteger()
                                if self.ParseTokenNoValue(lex.TokenType.closeSquareBracket):
                                    block = self.ParseBlock()
                                    if block != None:
                                        if self.test: print('accessed node init in ParseFunctionDecl')
                                        return ast.ASTFunctionDeclNode(identifier, typeLiteral, block, params, integer)
                            else:
                                block = self.ParseBlock()
                                if block != None: return ast.ASTFunctionDeclNode(identifier, typeLiteral, block, params)

    def ParseNodeAndSemicolon(self, parseFunc):
        def aggregateFunc():
            node = parseFunc()
            if (node != None):
                if self.ParseTokenNoValue(lex.TokenType.semicolon):
                    return node
        return aggregateFunc
    
    def ParseStatement(self):
        if self.test: print('accessed ParseStatement')
        return self.ParseAnOption([
            self.ParseNodeAndSemicolon(self.ParseVariableDecl),
            self.ParseNodeAndSemicolon(self.ParseAssignment),
            self.ParseNodeAndSemicolon(self.ParsePrintStatement),
            self.ParseNodeAndSemicolon(self.ParseDelayStatement),
            self.ParseNodeAndSemicolon(self.ParseWriteStatement),
            self.ParseIfStatement,
            self.ParseForStatement,
            self.ParseWhileStatement,
            self.ParseNodeAndSemicolon(self.ParseRtrnStatement),
            self.ParseFunctionDecl,
            self.ParseBlock
        ])
        

    def ParseBlock(self):
        if self.test: print('accessed ParseBlock')
        if self.ParseTokenNoValue(lex.TokenType.openCurlyBracket):
            if self.test: print('accessed openCurlyBracket in ParseBlock')
            block = ast.ASTBlockNode()
            while not self.ParseTokenNoValue(lex.TokenType.closeCurlyBracket):
                statement = self.ParseStatement()
                if statement != None: block.add_statement(statement)
                else:
                    # print('fatal error. Exiting loop') # should never be accessed because of decision of while loop
                    # break
                    raise Exception('statement cannot be None')
            return block

    def ParseProgram(self):                        
        if self.test: print('accessed ParseProgram')
        self.NextToken()  #set crtToken to the first token (skip all WS)
        
        program = ast.ASTProgramNode()
        while (self.crtToken.type != lex.TokenType.end):
            statement = self.ParseStatement()
            if statement != None: program.add_statement(statement)
        return program
        # a statement which does not end with semicolon is omitted but no error is shown.

    def Parse(self):        
        self.ASTroot = self.ParseProgram()


def driverCode():
    #parser = Parser("x=23;")
    # parser = Parser("let x=   23 ; y=  100; { z = 23 ;xy=3; } fun hello()->bool{return 2;} x=hello()+2*3/6-2*(8-4);")
    # parser = Parser("x = hello();")
##    parser.test = True # test
##    parser = Parser("x=   23 ; y=  100;")
    parser = Parser('{ z = 23 ; xy=3; }')
    parser.Parse()

    print_visitor = PrintNodesVisitor()
    parser.ASTroot.accept(print_visitor)
if __name__ == '__main__':
    driverCode()