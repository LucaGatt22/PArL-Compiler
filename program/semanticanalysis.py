from parserblock import Parser # parse, can output (like a log)
from symboltable import SymbolTable
from printnodesvisitor import PrintNodesVisitor
from astvisitor import ASTVisitor

test = True
class SemanticAnalysisVisitor(ASTVisitor):
    def __init__(self):
        super().__init__()
        self.name = "Semantic Analysis Visitor"
        self.symboltable = SymbolTable()

    # def inc_tab_count(self):
    #     self.tab_count += 1

    # def dec_tab_count(self):
    #     self.tab_count -= 1
        
    def visit_integer_node(self, int_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Integer value::", int_node.value)
        return 'int'

    def visit_assignment_node(self, ass_node):
        self.node_count += 1     
        identifier = ass_node.id.accept(self)
        if test & (identifier == None): raise Exception('error None identifier') # test
        exprType = ass_node.expr.accept(self)
        symbolType = self.symboltable.lookupGetType(identifier)
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
        self.symboltable.push()
        for st in block_node.stmts:
            st.accept(self)


    def visit_program_node(self, program_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Program => ")
        self.inc_tab_count()
        
        for st in program_node.stmts:
            st.accept(self)
        
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
        
    def visit_bool_node(self, bool_node):
        self.visit_general(bool_node.value, "Bool", 'value')
        return 'bool'
    def visit_float_node(self, float_node):
        self.visit_general(float_node.value, "Float", 'value')
        return 'float'
    def visit_colour_node(self, colour_node):
        self.visit_general(colour_node.value, "Colour", 'value')
        return 'colour'
    def visit_padwidth_node(self): # return width in pixels (integer)
        self.visit_general(None, "PadWidth", 'cmd')
        return 'int'
    def visit_padheight_node(self): # return hieght in pixels (integer)
        self.visit_general(None, "PadHeight", 'cmd')
        return 'int'

    def visit_padread_node(self, padread_node):
        self.node_count += 1
        print('\t' * self.tab_count, "PadRead node => ")
        self.inc_tab_count()
        padread_node.exprX.accept(self)
        padread_node.exprY.accept(self)
        if (exprXType != 'int') | (exprYType != 'int'): raise Exception("Expected 'int' in __read statement.")
        self.dec_tab_count()

    def visit_padrandi_node(self, node):
        self.visit_general(node.value, "PadRandI", 'parent')

    def visit_identifierarray_node(self, identifierarray_node):
        self.node_count += 1
        print('\t' * self.tab_count, "IdentifierArray node => ")
        self.inc_tab_count()
        identifierarray_node.variable.accept(self)
        identifierarray_node.index.accept(self)
        self.dec_tab_count()

        raise NotImplementedError('did not implement visit_identifierarray_node() for semantic analysis\n\tYet to implement check for array out of bounds')

    def visit_multiop_node(self, multiop_node):
        self.visit_general(multiop_node.operationValue, "MultiplicativeOp", 'value')
    def visit_addop_node(self, addop_node):
        self.visit_general(addop_node.operationValue, "AdditiveOp", 'value')
    def visit_relop_node(self, addop_node):
        self.visit_general(relop_node.operationValue, "RelationalOp", 'value')

    def visit_oneListAttribute(self, childNodes, nodeName:str):
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+" node => ")
        self.inc_tab_count()
        elemTypePrev = None
        for elem in childNodes:
            elemType = elem.accept(self)
            if test: print("elemTypePrev="+str(elemTypePrev)) # test
            if test: print("elemType="+str(elemType)) # test
            if elemTypePrev == None: elemTypePrev = elemType
            if test: print("elemTypePrev="+str(elemTypePrev)) # test
            if test: print("elemType="+str(elemType)) # test
            if elemType != elemTypePrev: raise Exception(f"Operands' types mismatch: {elemType} with {elemTypePrev}")
        self.dec_tab_count()
        return elemType
    def visit_actualparams_node(self, acParams_node):
        visit_oneListAttribute(acParams_node.exprs, "ActualParams") # no type checking needed
        
    def visit_functioncall_node(self, functioncall_node):
        self.node_count += 1
        print('\t' * self.tab_count, "FunctionCall node => ")
        self.inc_tab_count()
        identifier = functioncall_node.identifier.accept(self)
        symbolType = self.symboltable.lookupGetType(identifier)
        if (symbolType == None) | (not symbolType.startswith('function')): raise Exception("Function {identifier} does not exist") # did not implement function/method signature in type check

        formalParamsTypes, returnType = symbolType.split(' -> ')
        formalParamsTypes = formalParamsTypes[10:-1] # remove 'function', '<' and '>'
        if functioncall_node.actualParams == None: actualParamsTypes = []
        else: actualParamsTypes = functioncall_node.actualParams.accept(self)
        formalParamsTypes = formalParamsTypes.split(', ')
        if formalParamsTypes == ['']: formalParamsTypes = []
        if test: print("formalParamsTypes="+str(formalParamsTypes)) # test
        if len(actualParamsTypes) != len(formalParamsTypes):
            raise Exception(f"Number of parameters mismatch for function '{identifier}'. Expected {len(formalParamsTypes)}, got {len(actualParamsTypes)}.")

        for actualParamsType, formalParamsType in zip(actualParamsTypes, formalParamsTypes):
            if actualParamsType != formalParamsType:
                raise Exception(f"Type mismatch for parameter in function '{identifier}'. Expected '{formalParamsType}', got '{actualParamsType}'.")
        
        self.dec_tab_count()
        return returnType

    def visit_subexpr_node(self, subexpr_node):
        exprType = self.visit_general(subexpr_node.expr, "SubExpr", 'parent')
        return exprType

    def visit_unary_node(self, node):
        # commented code assumes '-' and 'not' are the same # self.visit_general(node.expr, "Unary", 'parent')
        self.node_count += 1
        print('\t' * self.tab_count, str(nodeName)+"Unary node => ")
        self.inc_tab_count()
        print('\t' * self.tab_count, "Unary operator::", node.unaryOp)
        exprType = node.expr.accept(self)
        self.dec_tab_count()
        return exprType

    def visit_factor_node(self, node):
        exprType = self.visit_general(node.childNode, "Factor", 'parent')
        return exprType

    def visit_term_node(self, node):
        exprType = self.visit_oneListAttribute(node.nodes, 'Term')
        return exprType

    def visit_simpleexpr_node(self, node):
        exprType = self.visit_oneListAttribute(node.nodes, 'SimpleExpr')
        return exprType

    def visit_expr_node(self, node):
        exprType = self.visit_oneListAttribute(node.nodes, 'Expr')
        
        # as clause
        if node.typeLiteral != None:
            self.inc_tab_count()
            print('\t' * self.tab_count, "Type node =>")
            typeLiteral = node.typeLiteral.accept(self)
            self.dec_tab_count()
            return typeLiteral
        return exprType


    def visit_variabledecl_node(self, variabledecl_node):
        self.node_count += 1
        
        identifier = variabledecl_node.identifier.accept(self)
        symbolType = variabledecl_node.variableDeclSuffix.accept(self)

        if self.symboltable.lookupCurrentFrame(identifier) != None: raise Exception(f'Variable or function {identifier} already initialised')
        self.symboltable.insert(identifier, symbolType)

    def visit_variabledeclsuffix_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "VariableDeclSuffix node => ")
        self.inc_tab_count()
        try: # https://www.w3schools.com/python/python_try_except.asp
            variableDeclArrayType = node.variableDeclArray.accept(self) # return value probably not worked
            return variableDeclArrayType
        except AttributeError: # if variableDeclArray does not exist in the node instance
            symbolType = node.typeLiteral.accept(self)
            exprType = node.expr.accept(self)
            if symbolType != exprType: raise Exception(f"TypeConflictError: Expression type does not match with variable type. symbolType={symbolType}, exprType={exprType}")
            return symbolType
        self.dec_tab_count() # not reached due to return statements

    def visit_variabledeclarray_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "VariableDeclArray node => ")
        self.inc_tab_count()
        try:
            node.integerLiteral.accept(self)
            node.literal.accept(self)
        except AttributeError:
            for elem in node.literals:
                elem.accept(self)
        self.dec_tab_count()

    def visit_requireexpr_node(self, node, nodeName):
        exprType = self.visit_general(node.expr, nodeName, 'parent')
        return exprType
    def visit_printstatement_node(self, node):
        exprType = self.visit_requireexpr_node(node, "PrintStatement") # can be of any type
    def visit_delaystatement_node(self, node):
        exprType = self.visit_requireexpr_node(node, "DelayStatement")
        if exprType != 'int': raise Exception('Expected int in DelayStatement')

    def visit_writestatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "WriteStatement node => ")
        self.inc_tab_count()
        for expr in node.exprs:
            node.expr.accept(self)
        self.dec_tab_count()

    def visit_rtrnstatement_node(self, node):
        exprType = self.visit_requireexpr_node(node, "RtrnStatement")
        return exprType

    def visit_ifstatement_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "IfStatement node => ")
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
        print('\t' * self.tab_count, "FormalParam node => ")
        self.inc_tab_count()
        name = node.identifier.accept(self)
        symbolType = node.typeLiteral.accept(self)
        if node.integerLiteral != None: node.integerLiteral.accept(self)
        self.dec_tab_count()

        self.symboltable.insert(name, symbolType)
        return symbolType

    def visit_formalparams_node(self, node):
        self.node_count += 1
        print('\t' * self.tab_count, "FormalParams node => ")
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
        formalParamsTypes = "<>"
        if node.formalParams != None: formalParamsTypes = node.formalParams.accept(self) # symbol inserts in formalParams
        node.typeLiteral.accept(self)
        if node.integerLiteral != None: node.integerLiteral.accept(self)
        returnType = node.block.accept(self)
        self.dec_tab_count()
        
        symbolType = f'function {formalParamsTypes} -> {returnType}'
        self.symboltable.insert(name, symbolType)


def driverCode():
    # parser driver code
##    parser = Parser("x=23;")
##    parser = Parser("let x: int =   23 ; y=  100; { z = 23 ;xy=3; } fun hello()->bool{return 2;} x=hello()+2*3/6-2*(8-4);")
##    parser = Parser("fun hello()->bool{return 2;} x=hello()+2*3/6-2*(8-4);")
##    parser = Parser("x = hello();")
##    parser.test = True # test
##    parser = Parser("x=   23 ; y=  100;")
    parser = Parser('{ let z:int = 23 ; let xy: int =3; }')
    parser.Parse()

    

    # semantic analysis (visitor)
    semantic_visitor = SemanticAnalysisVisitor()
    parser.ASTroot.accept(semantic_visitor)



    # print nodes visitor
    print_visitor = PrintNodesVisitor()
    parser.ASTroot.accept(print_visitor)
if __name__ == '__main__':
    test = False
    driverCode()
