from parserblock import Parser # parse, can output (like a log)
from symboltable import SymbolTable
from printnodesvisitor import PrintNodesVisitor
from astvisitor import ASTVisitor
from semanticanalysis import SemanticAnalysisVisitor

class CodeGenerationVisitor(ASTVisitor):
    def __init__(self):
        super().__init__()
        self.name = "Code Generation Visitor"
        self.symboltable = SymbolTable()
        self.instructionsGlobal = []
        self.node_count = 0
        self.block_count = 0
        
    def appendToFile(self, data):
        with open('generatedcode.parir', 'a') as file:
            file.write(data + '\n')
        # print("Data appended successfully.")

    def storeInstructionsInFile(self, instructions):
        for instruction in instructions:
            self.appendToFile(instruction)

    def appendInstruction(self, instruction):
        self.instructionsGlobal.append(instruction)
        # self.appendInstruction(instruction)

    def appendBlockInstructionsToParent(parentInstructions, blockInstructions): # not used
        for instruction in blockInstructions:
            appendInstruction(instruction)

    def visit_integer_node(self, int_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Integer value::", int_node.value)
        instruction = f'push {int_node.value}'
        self.appendInstruction(instruction)
        return instruction

    def visit_assignment_node(self, ass_node):
        self.node_count += 1     
        identifier = ass_node.id.accept(self)
        ass_node.expr.accept(self)
        # self.appendInstruction(f'push {exprValue}') # c
        symbolValueAddr = self.symboltable.lookupGetValueAddr(identifier)
        self.appendInstruction(f'push {symbolValueAddr.symbolIndex}') # b
        self.appendInstruction(f'push {symbolValueAddr.frameIndex}') # a
        self.appendInstruction('st')

        return [
            f'push {symbolValueAddr.symbolIndex}',
            f'push {symbolValueAddr.frameIndex}',
            'st'
        ]
        

    visit_variable_node = lambda self, var_node: self.visit_identifier_node(var_node) # alias
    def visit_identifier_node(self, var_node):
        self.node_count += 1
        if test & (var_node.lexeme == None): raise Exception('error None identifier') # test
        try:
            valueAddr = symboltable.lookupGetValueAddr(var_node.lexeme)
            return f'push [{valueAddr.symbolIndex}:{valueAddr.frameIndex}]' # [i:I]
        except Exception: pass # return [i:I] if variable or function exists

        return var_node.lexeme

    def visit_block_node(self, block_node):
        self.node_count += 1
        self.block_count += 1 # to identify block function name, such as block1
        # self.appendInstruction(f'push {len(block_node.stmts)}') # length of ParL statements does not equal to PArIR statements
        instructions = []
        instructions.append('push 0') # space is allocated one space at a time whenever an 'st' (store) is found
        instructions.append('oframe') # oframe and cframe are supposed to be used for memory stack
        self.symboltable.push()
        for st in block_node.stmts:
            instructions.append( st.accept(self) )
            if returnValue != None: break
        instructions.append('cframe')
        return instructions


    def visit_program_node(self, program_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Program => ")
        self.inc_tab_count()
        
        self.appendInstruction('.main')
        instructions = ['.main']
        for st in program_node.stmts:
            instructions.append(st.accept(self))
        self.appendInstruction('halt')
        
        self.dec_tab_count()

        return instructions + ['halt']

    def visit_type_node(self, type_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Type value::", type_node.typeLiteral)
        
        return type_node.typeLiteral

    def visit_literal_node(self, literal_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Literal node => ")
        self.inc_tab_count()
        literal_node.literal.accept(self)
        self.dec_tab_count()


    '''
    visit_general(self, attrNode, nodeTypeStr, isParentNode)
    A general function for visiting nodes
    attrNode - the paramater is the attribute of the node instead of the node being visited
        The attribute is either a child (if non-terminal) or data (if terminal).
    nodeNameStr - the name of the node. Should start with a capital letter
    isParentNode - boolean to decide if terminal or non-terminal
    '''
    def visit_general(self, attrNode, nodeNameStr: str, isParentNode_Value_Cmd: str):
        self.node_count += 1
        if isParentNode_Value_Cmd == 'parent':
            print('\t' * self.tab_count, nodeNameStr+" node => ")
            self.inc_tab_count()
            attrNode.accept(self) # Should it be called exprType?
            self.dec_tab_count()
            
        elif isParentNode_Value_Cmd == 'value':
            print('\t' * self.tab_count, nodeNameStr+" value::", attrNode)
        elif isParentNode_Value_Cmd == 'cmd':
            print('\t' * self.tab_count, nodeNameStr+" command", attrNode)
        else: print('ERROR: '+str(isParentNode_Value_Cmd)+' does not exist.')
        
    def visit_bool_node(self, bool_node):
        self.visit_general(bool_node.value, "Bool", 'value')
        return bool_node.value
    def visit_float_node(self, float_node):
        self.visit_general(float_node.value, "Float", 'value')
        self.appendInstruction(f'push {float_node.value}')
        return float_node.value
    def visit_colour_node(self, colour_node):
        self.visit_general(colour_node.value, "Colour", 'value')
        self.appendInstruction(f'push {colour_node.value}')
        return colour_node.value
    def visit_padwidth_node(self):
        self.visit_general(None, "PadWidth", 'cmd')
        self.appendInstruction('width')
        return 'width'
    def visit_padheight_node(self):
        self.visit_general(None, "PadHeight", 'cmd')
        self.appendInstruction('height')
        return 'height'

    def visit_padread_node(self, padread_node):
        self.node_count += 1
        print('\t' * self.tab_count, "PadRead node => ")
        self.inc_tab_count()
        padread_node.exprX.accept(self)
        padread_node.exprY.accept(self)
        self.dec_tab_count()

    def visit_padrandi_node(self, node):
        self.visit_general(node.value, "PadRandI", 'parent')
        self.appendInstruction('irnd')
        return 'irnd'

    def visit_identifierarray_node(self, identifierarray_node):
        self.node_count += 1
        print('\t' * self.tab_count, "IdentifierArray node => ")
        self.inc_tab_count()
        identifierarray_node.variable.accept(self)
        identifierarray_node.index.accept(self)
        self.dec_tab_count()

    def visit_multiop_node(self, multiop_node):
        self.visit_general(multiop_node.operationValue, "MultiplicativeOp", 'value')

        if multiop_node.operationValue == '*':
            self.appendInstruction('mul')
            return 'mul'
        elif multiop_node.operationValue == '/':
            self.appendInstruction('div')
            return 'div'
        elif multiop_node.operationValue == 'and':
            self.appendInstruction('and')
            return 'and'
    def visit_addop_node(self, addop_node):
        self.visit_general(addop_node.operationValue, "AdditiveOp", 'value')

        if addop_node.operationValue == '+':
            self.appendInstruction('add')
            return 'add'
        elif addop_node.operationValue == '-':
            self.appendInstruction('sub')
            return 'sub'
        elif addop_node.operationValue == 'or':
            self.appendInstruction('or')
            return 'or'
    def visit_relop_node(self, relop_node):
        self.visit_general(relop_node.operationValue, "RelationalOp", 'value')

        if relop_node.operationValue == '<':
            self.appendInstruction('lt')
            return 'lt'
        elif relop_node.operationValue == '>':
            self.appendInstruction('gt')
            return 'gt'
        elif relop_node.operationValue == '==':
            self.appendInstruction('eq')
            return 'eq'
        elif relop_node.operationValue == '!=': # same as inversion of the boolean returned by ==
            self.appendInstruction('eq')
            self.appendInstruction('not') # inversion of boolean i.e. (0 to 1) and (1 to 0)
            return ['eq', 'not']
        elif relop_node.operationValue == '<=':
            self.appendInstruction('le')
            return 'le'
        elif relop_node.operationValue == '>=':
            self.appendInstruction('ge')
            return 'ge'


    def visit_oneListAttribute(self, childNodes, nodeName:str):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+" node => ")
        self.inc_tab_count()
        for elem in childNodes:
            elem.accept(self)
        self.dec_tab_count()
    def visit_actualparams_node(self, acParams_node):
        visit_oneListAttribute(acParams_node.exprs, "ActualParams")
        
    def visit_functioncall_node(self, functioncall_node):
        self.node_count += 1
        print('\t' * self.tab_count, "FunctionCall node => ")
        self.inc_tab_count()
        identifier = functioncall_node.identifier.accept(self)
        self.appendInstruction(f'push .{identifier}')
        self.appendInstruction('call')
        if functioncall_node.actualParams != None: functioncall_node.actualParams.accept(self)
        self.dec_tab_count()

        return [ f'push .{identifier}', 'call' ]

    def visit_subexpr_node(self, subexpr_node):
        self.visit_general(subexpr_node.expr, "SubExpr", 'parent')

    def visit_unary_node(self, node):
        # commented code assumes '-' and 'not' are the same # self.visit_general(node.expr, "Unary", 'parent')
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"Unary node => ")
        self.inc_tab_count()
        print('\t' * self.tab_count, "Unary operator::", node.unaryOp)
        node.expr.accept(self) # expr before VM's 'not' command
        self.dec_tab_count()

        if node.unaryOp == 'not':
            self.appendInstruction('not')
            return 'not'
        elif node.unaryOp == '-': raise Exception('- UnaryOperator is excluded in code generation.')

        

    def visit_factor_node(self, node):
        self.visit_general(node.childNode, "Factor", 'parent')
        return node.childNode
        

    def visit_node_list(self, node): # implements visit_node functions of term, simpleexpr, and expr
        instructions = [ node.nodes[0].accept(self) ] # operand
        for operatorChildIndex in range(1,len(node.nodes),2): # iterate over each pair of an operand and an operator
            instructions.append( node.nodes[operatorChildIndex+1].accept(self) ) # operand
            instructions.append( node.nodes[operatorChildIndex].accept(self) ) # operator
        return instructions
    def visit_term_node(self, node):
        # self.visit_oneListAttribute(node.nodes, 'Term')
        
        return self.visit_node_list(node)

    def visit_simpleexpr_node(self, node):
        # self.visit_oneListAttribute(node.nodes, 'SimpleExpr')
        
        return self.visit_node_list(node)

    def visit_expr_node(self, node): # will not work
        # self.visit_oneListAttribute(node.nodes, 'Expr')
        instructions = self.visit_node_list(node)

        # as clause
        if node.typeLiteral != None:
            self.inc_tab_count()
            print('\t' * self.tab_count, "Type::", node.typeLiteral)
            self.dec_tab_count()
            # return node.typeLiteral # do not need to handle typeLiteral since every type is converted to a number in VM

        return instructions
        

    def genCodeInsertVar(self, identifier, symbolType):
        self.appendInstruction('alloc 1') # allocate space for a new element in current frame 
        self.symboltable.insert(identifier, symbolType)
        symbolValueAddr = self.symboltable.lookupGetValueAddr(identifier)
        self.appendInstruction(f'push {symbolValueAddr.symbolIndex}') # b
        self.appendInstruction(f'push {symbolValueAddr.frameIndex}') # a
        self.appendInstruction('st')

        return [ # no instructions appended here since this function is a helper function, not a `visit_node()` function
            'alloc 1',
            f'push {symbolValueAddr.symbolIndex}', # b
            f'push {symbolValueAddr.frameIndex}', # a
            'st'
        ]

    def visit_variabledecl_node(self, variabledecl_node):
        self.node_count += 1
        
        identifier = variabledecl_node.identifier.accept(self)
        symbolType, exprInstructions = variabledecl_node.variableDeclSuffix.accept(self)
        
        return exprInstructions + self.genCodeInsertVar(identifier, symbolType)

    def visit_variabledeclsuffix_node(self, node):
        self.node_count += 1
        try: # https://www.w3schools.com/python/python_try_except.asp
            node.variableDeclArray.accept(self)
        except AttributeError: # if variableDeclArray does not exist in the node instance
            symbolType = node.typeLiteral.accept(self)
            exprInstructions = node.expr.accept(self)
            return symbolType, exprInstructions

    def visit_variabledeclarray_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"VariableDeclSuffix node => ")
        self.inc_tab_count()
        try:
            node.integerLiteral.accept(self)
            node.literal.accept(self)
        except AttributeError:
            for elem in node.literals:
                elem.accept(self)
        self.dec_tab_count()

    def visit_requireexpr_node(self, node, nodeName):
        self.visit_general(node.expr, nodeName, 'parent')
        # self.appendInstruction(f'push {node.expr}') # only terminals have push command
        
    def visit_printstatement_node(self, node):
        instructions = self.visit_requireexpr_node(node, "PrintStatement") # can be of any type
        self.appendInstruction('print')
        instructions.append('print')
        return instructions
    def visit_delaystatement_node(self, node):
        self.visit_requireexpr_node(node, "DelayStatement")
        self.appendInstruction('delay')
        instructions.append('delay')
        return instructions

    def visit_writestatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"WriteStatement node => ")
        self.inc_tab_count()
        instructions = [ node.expr.accept(self) for expr in node.exprs ]
        self.dec_tab_count()
        
        if len(node.exprs) == 3:
            self.appendInstruction('write')
            instructions.append('write')
            return instructions
        elif len(node.exprs) == 5:
            self.appendInstruction('writebox')
            instructions.append('writebox')
            return instructions
        else: raise Exception("FATAL ERROR")

    def visit_rtrnstatement_node(self, node):
        instructions = self.visit_requireexpr_node(node, "RtrnStatement")
        self.appendInstruction('ret')
        instructions.append('ret')
        return instructions
        

    def visit_ifstatement_node(self, node): # work in progress
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"IfStatement node => ")
        self.inc_tab_count()
        
        exprInstructions = node.expr.accept(self)
        # compare using expr
        linesNumIf = linesNumElse = None
        if (linesNumIf == linesNumElse) & (linesNumIf == None): raise Exception("FATAL ERROR - do not know how to get number of lines of if or else block")
        self.appendInstruction(f'push #PC+{len(ifBlockInstructions)+1}\ncjmp') # jump over if - +1 to jump over (exclude) the jump statement found after `blockIf`
        ifBlockInstructions = node.blockIf.accept(self)
        self.appendInstruction(f'push #PC+{len(elseBlockInstructions)}\njmp')
        if node.blockElse != None: elseBlockInstructions = node.blockElse.accept(self)

        self.dec_tab_count()
        return exprInstructions + [f'push #PC+{len(ifBlockInstructions)+1}\ncjmp'] + \
            ifBlockInstructions + [f'push #PC+{len(elseBlockInstructions)}\njmp'] + elseBlockInstructions
        # return ifStatInstructions

    def visit_forstatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"ForStatement node => ")
        self.inc_tab_count()
        if node.varDec != None: varDecInstructions = node.varDec.accept(self)
        exprInstructions = node.expr.accept(self)
        if node.assignment != None: assignmentInstructions = node.assignment.accept(self)
        blockInstructions = node.block.accept(self)
        self.dec_tab_count()

    def visit_whilestatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"WhileStatement node => ")
        self.inc_tab_count()
        exprInstructions = node.expr.accept(self) # b
        # f'push #PC+{len(blockInstructions)+1}\ncjmp'
        blockInstructions = node.block.accept(self) # a
        # f'push #PC-{len(blockInstructions)+1}\njmp' # jump to 'cjmp' (conditional jump)
        self.dec_tab_count()
        return exprInstructions + [f'push #PC+{len(blockInstructions+1)}\ncjmp'] + blockInstructions + [f'push #PC-{len(blockInstructions)+1}\njmp']

    
    def visit_formalparam_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"FormalParam node => ")
        self.inc_tab_count()
        identifier = node.identifier.accept(self)
        symbolType = node.typeLiteral.accept(self)
        if node.integerLiteral != None: node.integerLiteral.accept(self)
        self.dec_tab_count()

        return self.genCodeInsertVar(identifier, symbolType)
        

    def visit_formalparams_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"FormalParams node => ")
        self.inc_tab_count()
        # self.appendInstruction(f'push {len(paramsInstructions)}')
        self.appendInstruction(f'oframe')
        paramsInstructions = []
        for formalparam in node.formalparams:
            paramsInstructions.append(formalparam.accept(self))
        self.dec_tab_count()
        return [f'push {len(paramsInstructions)}\noframe'] + paramsInstructions

    def visit_functiondecl_node(self, node):
        self.node_count += 1
        self.symboltable.push()
        print('\t' * self.tab_count, "FunctionDecl node => ")
        self.inc_tab_count()
        name = node.identifier.accept(self)
        # if self.symboltable.lookupCurrentFrame(name) != None: raise Exception(f'{name} already declared.') # check done in semantic analysis
        self.appendInstruction(f'\n.{name}') # newline to seperate between functions
        if node.formalParams != None: paramsInstructions = node.formalParams.accept(self) # symbol inserts in formalParams
        ''' # return type - semantic analysis so exclude
        returnType = node.typeLiteral.accept(self)
        if node.integerLiteral != None: node.integerLiteral.accept(self) # part of return type, so part of semantic analysis check
        '''
        blockInstructions = node.block.accept(self)
        self.dec_tab_count()
        
        symbolType = f'function {formalParamsTypes} -> {returnType}' # the type of a function is 'function' together with its signature
        self.symboltable.insert(name, symbolType)
        self.appendInstruction('cframe') # close frame opened in `visit_formalparams_node()`

        return [f'\n.{name}'] + paramsInstructions + blockInstructions + ['cframe']


def driverCode():
    # parser driver code
    #parser = Parser("x=23;")
    # parser = Parser("let x=   23 ; y=  100; { z = 23 ;xy=3; } fun hello()->bool{return 2;} x=hello()+2*3/6-2*(8-4);")
##    parser = Parser("x = hello();")
##    parser.test = True # test
##    parser = Parser("x=   23 ; y=  100;")
    parser = Parser('{ let z:int = 23 ; let xy:int; xy=3; }')
    parser.Parse()

    

    # semantic analysis (visitor)
    semantic_visitor = SemanticAnalysisVisitor()
    parser.ASTroot.accept(semantic_visitor)

    # code generation (visitor)
    codeGenerationVisitor = CodeGenerationVisitor()
    parser.ASTroot.accept(codeGenerationVisitor)

    # print nodes visitor
    print_visitor = PrintNodesVisitor()
    parser.ASTroot.accept(print_visitor)

if __name__ == '__main__':
    driverCode()