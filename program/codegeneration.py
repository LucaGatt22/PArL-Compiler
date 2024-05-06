from parserblock import Parser # parse, can output (like a log)
from symboltable import SymbolTable
from printnodesvisitor import PrintNodesVisitor
from astvisitor import ASTVisitor
from semanticanalysis import SemanticAnalysisVisitor

class CodeGenerationVisitor:
    def __init__(self):
        super().__init__()
        self.name = "Code Generation Visitor"
        self.symboltable = SymbolTable()
        
    def appendToFile(self, data):
        with open('generatedcode.parir', 'a') as file:
            file.write(data + '\n')
        # print("Data appended successfully.")

    def visit_integer_node(self, int_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Integer value::", int_node.value)
        appendToFile(f'push {int_node.value}')
        return 'int'

    def visit_assignment_node(self, ass_node):
        self.node_count += 1     
        identifier = ass_node.id.accept(self)
        exprType = ass_node.expr.accept(self)
        # appendToFile(f'push {exprValue}') # c
        symbolValueAddr = self.symboltable.lookupGetValueAddr(identifier)
        appendToFile(f'push {symbolValueAddr.symbolIndex}') # b
        appendToFile(f'push {symbolValueAddr.frameIndex}') # a
        appendToFile('st')
        if symbolType != exprType: raise Exception("TypeConflictError: Expression type does not match with variable type.")

    visit_variable_node = lambda self, var_node: self.visit_identifier_node(var_node) # alias
    def visit_identifier_node(self, var_node):
        self.node_count += 1
        if test & (var_node.lexeme == None): raise Exception('error None identifier') # test
        return var_node.lexeme
        '''
        cannot call a lookup function here since lookup function may differ if a new variable or function is declared or in the case of an assignment
        in case of assignment - if a value is assigned to a variable (or function is called)
        Hence commented code,
        # return symboltable.lookupGetType(var_node.lexeme)
        '''

    def visit_block_node(self, block_node):
        self.node_count += 1
        appendToFile(f'push {len(block_node.stmts)}')
        appendToFile('oframe')
        self.symboltable.push()
        for st in block_node.stmts:
            returnValue = st.accept(self)
            if returnValue != None: break
        appendToFile('cframe')
        return returnValue


    def visit_program_node(self, program_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Program => ")
        self.inc_tab_count()
        
        appendToFile('.main')
        for st in program_node.stmts:
            st.accept(self)
        appendToFile('halt')
        
        self.dec_tab_count()

    def visit_type_node(self, type_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Type value::", type_node.typeLiteral)
        
        return type_node.typeLiteral

    def visit_literal_node(self, literal_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Literal node => ")
        self.inc_tab_count()
        valueType = literal_node.literal.accept(self)
        self.dec_tab_count()
        return valueType


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
            exprType = attrNode.accept(self) # Should it be called exprType?
            self.dec_tab_count()
            return exprType
        elif isParentNode_Value_Cmd == 'value':
            print('\t' * self.tab_count, nodeNameStr+" value::", attrNode)
        elif isParentNode_Value_Cmd == 'cmd':
            print('\t' * self.tab_count, nodeNameStr+" command", attrNode)
        else: print('ERROR: '+str(isParentNode_Value_Cmd)+' does not exist.')
        
    def visit_bool_node(self, bool_node): # ?
        self.visit_general(bool_node.value, "Bool", 'value')
        return 'bool'
    def visit_float_node(self, float_node):
        self.visit_general(float_node.value, "Float", 'value')
        appendToFile(f'push {float_node.value}')
        return 'float'
    def visit_colour_node(self, colour_node):
        self.visit_general(colour_node.value, "Colour", 'value')
        appendToFile(f'push {colour_node.value}')
        return 'colour'
    def visit_padwidth_node(self):
        self.visit_general(None, "PadWidth", 'cmd')
        appendToFile('width')
    def visit_padheight_node(self):
        self.visit_general(None, "PadHeight", 'cmd')
        appendToFile('height')

    def visit_padread_node(self, padread_node):
        self.node_count += 1
        print('\t' * self.tab_count, "PadRead node => ")
        self.inc_tab_count()
        padread_node.exprX.accept(self)
        padread_node.exprY.accept(self)
        if (exprXType != 'int') | (exprYType != 'int'): raise Exception("Expected int in __read statement.")
        self.dec_tab_count()

    def visit_padrandi_node(self, node):
        self.visit_general(node.value, "PadRandI", 'parent')
        appendToFile('irnd')

    def visit_identifierarray_node(self, identifierarray_node):
        self.node_count += 1
        print('\t' * self.tab_count, "IdentifierArray node => ")
        self.inc_tab_count()
        identifierarray_node.variable.accept(self)
        identifierarray_node.index.accept(self)
        self.dec_tab_count()

    def visit_multiop_node(self, multiop_node):
        self.visit_general(multiop_node.operationValue, "MultiplicativeOp", 'value')

        if multiop_node.operationValue == '*': appendToFile('mul')
        elif multiop_node.operationValue == '/': appendToFile('div')
        elif multiop_node.operationValue == 'and': appendToFile('and')
    def visit_addop_node(self, addop_node):
        self.visit_general(addop_node.operationValue, "AdditiveOp", 'value')

        if addop_node.operationValue == '+': appendToFile('add')
        elif addop_node.operationValue == '-': appendToFile('sub')
        elif addop_node.operationValue == 'or': appendToFile('or')
    def visit_relop_node(self, relop_node):
        self.visit_general(relop_node.operationValue, "RelationalOp", 'value')

        if relop_node.operationValue == '<': appendToFile('lt')
        elif relop_node.operationValue == '>': appendToFile('gt')
        elif relop_node.operationValue == '==': appendToFile('eq')
        elif relop_node.operationValue == '!=':
            appendToFile('eq')
            appendToFile('not')
        elif relop_node.operationValue == '<=': appendToFile('le')
        elif relop_node.operationValue == '>=': appendToFile('ge')


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
        symbolType = self.symboltable.lookupGetType(identifier)
        appendToFile(f'push .{identifier}')
        if functioncall_node.actualParams != None: functioncall_node.actualParams.accept(self)
        self.dec_tab_count()

        appendToFile('ret')

    def visit_subexpr_node(self, subexpr_node):
        exprType = self.visit_general(subexpr_node.expr, "SubExpr", 'parent')
        return exprType

    def visit_unary_node(self, node):
        # commented code assumes '-' and 'not' are the same # self.visit_general(node.expr, "Unary", 'parent')
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"Unary node => ")
        self.inc_tab_count()
        print('\t' * self.tab_count, "Unary operator::", node.unaryOp)
        exprType = node.expr.accept(self) # expr before VM's 'not' command
        self.dec_tab_count()

        if node.unaryOp == 'not': appendToFile('not')
        elif node.unaryOp == '-': raise Exception('- UnaryOperator is excluded in code generation.')

        return exprType

    def visit_factor_node(self, node):
        exprType = self.visit_general(node.childNode, "Factor", 'parent')
        return exprType

    def visit_node_list(self, node):
        node.nodes[0].accept(self) # operand
        for operatorChildIndex in range(1,len(node.nodes),2): # iterate over each pair of an operand and an operator
            node.nodes[operatorChildIndex+1].accept(self) # operand
            node.nodes[operatorChildIndex].accept(self) # operator
    def visit_term_node(self, node):
        # exprType = self.visit_oneListAttribute(node.nodes, 'Term')
        # return exprType
        self.visit_node_list(node)

    def visit_simpleexpr_node(self, node):
        # exprType = self.visit_oneListAttribute(node.nodes, 'SimpleExpr')
        # return exprType
        self.visit_node_list(node)

    def visit_expr_node(self, node): # will not work
        # exprType = self.visit_oneListAttribute(node.nodes, 'Expr')
        self.visit_node_list(node)

        # as clause
        if node.typeLiteral != None:
            self.inc_tab_count()
            print('\t' * self.tab_count, "Type::", node.typeLiteral)
            self.dec_tab_count()
            return node.typeLiteral
        return exprType


    def visit_variabledecl_node(self, variabledecl_node):
        self.node_count += 1
        
        identifier = variabledecl_node.identifier.accept(self)
        symbolType = variabledecl_node.variableDeclSuffix.accept(self)

        if self.symboltable.lookupCurrentFrame(identifier) != None: raise Exception(f'Variable or function {identifier} already initialised')
        self.symboltable.insert(identifier, symbolType)

    def visit_variabledeclsuffix_node(self, node):
        self.node_count += 1
        try: # https://www.w3schools.com/python/python_try_except.asp
            node.variableDeclArray.accept()
        except AttributeError: # if variableDeclArray does not exist in the node instance
            symbolType = node.typeLiteral.accept()
            exprType = node.expr.accept()
            if symbolType != exprType: raise Exception("TypeConflictError: Expression type does not match with variable type.")
            return symbolType

    def visit_variabledeclarray_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"VariableDeclSuffix node => ")
        self.inc_tab_count()
        try:
            node.integerLiteral.accept()
            node.literal.accept()
        except AttributeError:
            for elem in node.literals:
                elem.accept()
        self.dec_tab_count()

    def visit_requireexpr_node(self, node, nodeName):
        exprType = self.visit_general(node.expr, nodeName, 'parent')
        # appendToFile(f'push {node.expr}') # only terminals have push command
        return exprType
    def visit_printstatement_node(self, node):
        exprType = self.visit_requireexpr_node(node, "PrintStatement") # can be of any type
        appendToFile('print')
    def visit_delaystatement_node(self, node):
        exprType = self.visit_requireexpr_node(node, "DelayStatement")
        appendToFile('delay')

    def visit_writestatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"WriteStatement node => ")
        self.inc_tab_count()
        for expr in node.exprs:
            node.expr.accept()
        self.dec_tab_count()
        
        if len(node.exprs) == 3: appendToFile('write')
        elif len(node.exprs) == 5: appendToFile('writebox')
        else: raise Exception("FATAL ERROR")

    def visit_rtrnstatement_node(self, node):
        exprType = self.visit_requireexpr_node(node, "RtrnStatement")
        return exprType

    def visit_ifstatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"IfStatement node => ")
        self.inc_tab_count()
        exprType = node.expr.accept(self)
        node.blockIf.accept(self)
        if node.blockElse != None: node.blockElse.accept(self)
        self.dec_tab_count()
        
        if exprType != 'bool': raise Exception('Expected bool in IfStatement')

    def visit_forstatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"ForStatement node => ")
        self.inc_tab_count()
        if node.varDec != None: node.varDec.accept(self)
        exprType = node.expr.accept(self)
        if node.assignment != None: node.assignment.accept(self)
        node.block.accept(self)
        self.dec_tab_count()

        if exprType != 'bool': raise Exception('Expected bool in ForStatement')

    def visit_whilestatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"WhileStatement node => ")
        self.inc_tab_count()
        exprType = node.expr.accept(self)
        node.block.accept(self)
        self.dec_tab_count()

        if exprType != 'bool': raise Exception('Expected bool in WhileStatement')

    def visit_formalparam_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"FormalParam node => ")
        self.inc_tab_count()
        name = node.identifier.accept(self)
        symbolType = node.typeLiteral.accept(self)
        if node.integerLiteral != None: node.integerLiteral.accept(self)
        self.dec_tab_count()

        self.symboltable.insert(name, symbolType)
        return symbolType

    def visit_formalparams_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"FormalParams node => ")
        self.inc_tab_count()
        formalParamsTypes = '<'
        for formalparam in node.formalparams:
            formalParamsTypes += str(formalparam.accept(self)) + ', '
        formalParamsTypes = formalParamsTypes[:-2] + '>'
        self.dec_tab_count()
        return formalParamsTypes

    def visit_functiondecl_node(self, node):
        self.node_count += 1
        self.symboltable.push()
        print('\t' * self.tab_count, "FunctionDecl node => ")
        self.inc_tab_count()
        name = node.identifier.accept(self)
        if self.symboltable.lookupCurrentFrame(name) != None: raise Exception(f'{name} already declared.')
        if node.formalParams != None: formalParamsTypes = node.formalParams.accept(self) # symbol inserts in formalParams
        node.typeLiteral.accept(self)
        if node.integerLiteral != None: node.integerLiteral.accept(self)
        returnType = node.block.accept(self)
        self.dec_tab_count()
        
        symbolType = f'function {formalParamsTypes} -> {returnType}'
        self.symboltable.insert(name, symbolType)


if __name__ == '__main__':
    # parser driver code
    #parser = Parser("x=23;")
    parser = Parser("let x=   23 ; y=  100; { z = 23 ;xy=3; } fun hello()->bool{return 2;} x=hello()+2*3/6-2*(8-4);")
##    parser = Parser("x = hello();")
##    parser.test = True # test
##    parser = Parser("x=   23 ; y=  100;")
##    parser = Parser('{ z = 23 ; xy=3; }')
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