#First some AST Node classes we'll use to build the AST with
class ASTNode:
    def __init__(self):
        self.name = "ASTNode"    

##class ASTStatementNode(ASTNode):
##    def __init__(self):
##        self.name = "ASTStatementNode"
class ASTStatementNode(ASTNode):
    def __init__(self, statementSpecific):
        self.name = "ASTStatementNode"
        self.statementSpecific = statementSpecific

    def accept(self, visitor):
        visitor.visit_statement_node(self)


class ASTExprNode():#ASTTermNode): # to replace ASTExpressionNode
    def __init__(self, simpleExpr, typeLiteral=None):
        self.name = "ASTExprNode"
        self.nodes = [simpleExpr] # children
        self.typeLiteral = typeLiteral

    def add_simpleexpr(self, relOp, simpleExpr):
        self.nodes.append(relOp)
        self.nodes.append(simpleExpr)

    def add_type(self, typeLiteral):
        self.typeLiteral = typeLiteral
        
    def accept(self, visitor):
        visitor.visit_expr_node(self)
class ASTExpressionNode(ASTExprNode): # alias
    pass
##    def __init__(self):
##        self.name = "ASTExpressionNode"

class ASTVariableNode(ASTExpressionNode): # identifier
    def __init__(self, lexeme):
        self.name = "ASTVariableNode"
        self.lexeme = lexeme

    def accept(self, visitor):
        visitor.visit_variable_node(self)

class ASTIntegerNode(ASTExpressionNode): # ASTLiteralNode
    def __init__(self, v):
        self.name = "ASTIntegerNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_integer_node(self)        

class ASTAssignmentNode(ASTStatementNode):
    def __init__(self, ast_var_node, ast_expression_node):
        self.name = "ASTAssignmentNode"        
        self.id   = ast_var_node
        self.expr = ast_expression_node

    def accept(self, visitor):
        visitor.visit_assignment_node(self)                

class ASTBlockNode(ASTNode):
    def __init__(self):
        self.name = "ASTBlockNode"
        self.stmts = [] # blocks of statements are the children of blocks

    def add_statement(self, node):
        self.stmts.append(node)

    def accept(self, visitor):
        visitor.visit_block_node(self)


class ASTProgramNode(ASTBlockNode):
    def __init__(self):
        self.name = "ASTProgramNode"
        self.stmts = [] # blocks of statements are the children of blocks

##    def add_statement(self, node):
##        self.stmts.append(node)

    def accept(self, visitor):
        visitor.visit_program_node(self) # program is a block


class ASTTypeNode(ASTNode):
    def __init__(self, typeLiteral):
        self.name = "ASTTypeNode"        
        self.typeLiteral = typeLiteral #  ‘float’ | ‘int’ | ‘bool’ | ‘colour’

    def accept(self, visitor):
        visitor.visit_type_node(self)

class ASTLiteralNode(ASTExpressionNode):
    def __init__(self, literal):
        self.name = "ASTLiteralNode"
        self.literal = literal # bool,int,float,colour,padwidth,padheight,padRead

    def accept(self, visitor):
        visitor.visit_literal_node(self)

class ASTBoolNode(ASTLiteralNode):
    def __init__(self, value):
        self.name = "ASTLiteralNode"
        self.value = value # true, false

    def accept(self, visitor):
        visitor.visit_bool_node(self)

class ASTFloatNode(ASTLiteralNode):
    def __init__(self, v):
        self.name = "ASTFloatNode"
        self.value = v

    def accept(self, visitor):
        visitor.visit_float_node(self)

class ASTColourNode(ASTLiteralNode):
    def __init__(self, v):
        self.name = "ASTColourNode"
        self.value = v # 6-digit value without '#'

    def accept(self, visitor):
        visitor.visit_colour_node(self)

class ASTPadWidthNode(ASTLiteralNode):
    def __init__(self):
        self.name = "ASTPadWidthNode"

    def accept(self, visitor):
        visitor.visit_padwidth_node(self)

class ASTPadHeightNode(ASTLiteralNode):
    def __init__(self):
        self.name = "ASTPadHeightNode"

    def accept(self, visitor):
        visitor.visit_padheight_node(self)

class ASTPadReadNode(ASTLiteralNode):
    def __init__(self, exprX, exprY):
        self.name = "ASTPadReadNode"
        self.exprX = exprX
        self.exprY = exprY

    def accept(self, visitor):
        visitor.visit_padread_node(self)

class ASTAbstractRequireExprNode(ASTNode): # abstract - not a node
    def __init__(self, expr):
        self.name = "error - abstract class"
        self.expr = expr

    def accept(self, visitor):
        visitor.visit_requireexpr_node(self)
        
class ASTPadRandINode(ASTNode):
    def __init__(self, exprLeft, expr):
        self.name = "ASTPadReadNode"
        self.exprLeft = exprLeft
        self.exprRight = exprRight

    def accept(self, visitor):
        visitor.visit_padread_node(self)

class ASTIdentifierArrayNode(ASTVariableNode):
    def __init__(self, variable, index:ASTExprNode):
        self.name = "ASTIdentifierArrayNode"
        self.variable = variable
        self.index = index # expr

    def accept(self, visitor):
        visitor.visit_identifierarray_node(self)

class ASTMultiOpNode(ASTNode):
    def __init__(self, operationValue):
        self.name = "ASTMultiOpNode"
        self.operationValue = operationValue

    def accept(self, visitor):
        visitor.visit_multiop_node(self)

class ASTAddOpNode(ASTNode):
    def __init__(self, operationValue):
        self.name = "ASTAddOpNode"
        self.operationValue = operationValue

    def accept(self, visitor):
        visitor.visit_addop_node(self)

class ASTRelOpNode(ASTNode):
    def __init__(self, operationValue):
        self.name = "ASTRelOpNode"
        self.operationValue = operationValue

    def accept(self, visitor):
        visitor.visit_relop_node(self)

class ASTActualParamsNode(ASTNode):
    def __init__(self, expr):
        self.name = "ASTActualParamsNode"
        self.exprs = [expr]

    def add_expr(self, expr_node):
        self.expr.append(expr_node)

    def accept(self, visitor):
        visitor.visit_actualparams_node(self)

class ASTFunctionCallNode(ASTNode):
    def __init__(self, iden, acParams=None):
        self.name = "ASTFunctionCallNode"
        self.identifier = iden # variable
        self.actualParams = acParams

    def accept(self, visitor):
        visitor.visit_functioncall_node(self)

class ASTSubExprNode(ASTExpressionNode):
    def __init__(self, expr):
        self.name = "ASTSubExprNode"
        self.expr = expr
        
    def accept(self, visitor):
        visitor.visit_subexpr_node(self)

class ASTUnaryNode(ASTExpressionNode):
    def __init__(self, unaryOp, expr):
        self.name = "ASTUnaryNode"
        self.unaryOp = unaryOp
        self.expr = expr

    def accept(self, visitor):
        visitor.visit_unary_node(self)

class ASTFactorNode(ASTExpressionNode):
    def __init__(self, childNode):
        self.name = "ASTFactorNode"
        self.childNode = childNode

    def accept(self, visitor):
        visitor.visit_factor_node(self)

class ASTTermNode(ASTExpressionNode):
    def __init__(self, factor):
        self.name = "ASTTermNode"
        self.nodes = [factor] # children

    def add_factor(self, multiOp, factor):
        self.nodes.append(multiOp)
        self.nodes.append(factor)

    def accept(self, visitor):
        visitor.visit_term_node(self)

class ASTSimpleExprNode(ASTTermNode):
    def __init__(self, term):
        self.name = "ASTSimpleExprNode"
        self.nodes = [term] # children

    def add_term(self, addOp, term):
        self.nodes.append(addOp)
        self.nodes.append(term)

    def accept(self, visitor):
        visitor.visit_simpleexpr_node(self)


##class ASTAssignmentNode(ASTTermNode): # already done

class ASTVariableDeclNode(ASTNode):
    def __init__(self, identifier, variableDeclSuffix):
        self.name = "ASTVariableDeclNode"
        self.identifier = identifier # children
        self.variableDeclSuffix = variableDeclSuffix

    def accept(self, visitor):
        visitor.visit_variabledecl_node(self)

class ASTVariableDeclSuffixNode(ASTNode):
    def __init__(self):
        self.name = "ASTVariableDeclSuffixNode"
    def __init__(self, typeLiteral, expr):
        __init__(self)
        self.typeLiteral = typeLiteral # children
        self.expr = expr
    def __init__(self, variableDeclArray):
        __init__(self)
        self.variableDeclArray = variableDeclArray

    def accept(self, visitor):
        visitor.visit_variabledeclsuffix_node(self)

class ASTVariableDeclArrayNode(ASTNode):
    def __init__(self):
        self.name = "ASTVariableDeclArrayNode"
    def __init__(self, integerLiteral, literal):
        __init__(self)
        self.integerLiteral = integerLiteral # children
        self.literal = literal
    def __init__(self, literal):
        __init__(self)
        self.literals = [literal]

    def addLiteral(self, literal):
        if (self.literals != None): self.literals.append(literal)

    def accept(self, visitor):
        visitor.visit_variabledeclarray_node(self)
        
class ASTPrintStatementNode(ASTAbstractRequireExprNode):
    def __init__(self, expr):
        self.name = "ASTPrintStatementNode"
        self.expr = expr

    def accept(self, visitor):
        visitor.visit_printstatement_node(self)

class ASTDelayStatementNode(ASTAbstractRequireExprNode):
    def __init__(self, expr):
        self.name = "ASTDelayStatementNode"
        self.expr = expr

    def accept(self, visitor):
        visitor.visit_delaystatement_node(self)

class ASTWriteStatementNode(ASTAbstractRequireExprNode):
    def __init__(self, expr1,expr2,expr3,expr4,expr5): # ‘__write_box’
        self.name = "ASTWriteStatementNode"
        self.exprs = [expr1,expr2,expr3,expr4,expr5]
    def __init__(self, expr1,expr2,expr3): # ‘__write’
        self.name = "ASTWriteStatementNode"
        self.exprs = [expr1,expr2,expr3]

    def accept(self, visitor):
        visitor.visit_writestatement_node(self)

class ASTRtrnStatementNode(ASTAbstractRequireExprNode): # return statement
    def __init__(self, expr):
        self.name = "ASTRtrnStatementNode"
        self.expr = expr

    def accept(self, visitor):
        visitor.visit_rtrnstatement_node(self)

class ASTIfStatementNode(ASTAbstractRequireExprNode):
    def __init__(self, expr, blockIf, blockElse=None):
        self.name = "ASTIfStatementNode"
        self.expr = expr
        self.blockIf = blockIf
        self.blockElse = blockElse

    def accept(self, visitor):
        visitor.visit_ifstatement_node(self)

class ASTForStatementNode(ASTAbstractRequireExprNode):
    def __init__(self, expr, block, varDec=None, assignment=None):
        self.name = "ASTForStatementNode"
        self.varDec = varDec
        self.expr = expr
        self.assignment = assignment
        self.block = block

    def accept(self, visitor):
        visitor.visit_forstatement_node(self)

class ASTWhileStatementNode(ASTAbstractRequireExprNode):
    def __init__(self, expr, block):
        self.name = "ASTWhileStatementNode"
        self.expr = expr
        self.block = block

    def accept(self, visitor):
        visitor.visit_whilestatement_node(self)

class ASTFormalParamNode(ASTAbstractRequireExprNode):
    def __init__(self, identifier, typeLiteral, integerLiteral=None):
        self.name = "ASTFormalParamNode"
        self.identifier = identifier
        self.typeLiteral = typeLiteral
        self.integerLiteral = integerLiteral

    def accept(self, visitor):
        visitor.visit_formalparam_node(self)

class ASTFormalParamsNode(ASTAbstractRequireExprNode):
    def __init__(self, formalparam):
        self.name = "ASTFormalParamsNode"
        self.formalparams = [formalparam]

    def addFormalParam(self, formalparam):
        self.formalparams.append(formalparam)

    def accept(self, visitor):
        visitor.visit_formalparams_node(self)

class ASTFunctionDeclNode(ASTAbstractRequireExprNode):
    def __init__(self, identifier, typeLiteral, block, formalParams=None, integerLiteral=None):
        self.name = "ASTFunctionDeclNode"
        self.identifier = identifier
        self.formalParams = formalParams
        self.typeLiteral = typeLiteral
        self.integerLiteral = integerLiteral
        self.block = block

    def accept(self, visitor):
        visitor.visit_functiondecl_node(self)




