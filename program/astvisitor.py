from astnodes import *

# interface class - functions are implemented in PrintNodesVisitor
class ASTVisitor:
    def __init__(self):
        self.name = "AST Visitor"
        self.node_count = 0
        self.tab_count = 0

    def inc_tab_count(self):
        self.tab_count += 1

    def dec_tab_count(self):
        self.tab_count -= 1
        
    def visit_integer_node(self, int_node):
        raise NotImplementedError()

    def visit_assignment_node(self, ass_node):
        raise NotImplementedError()

    visit_variable_node = lambda self, var_node: self.visit_identifier_node(var_node) # alias
    def visit_identifier_node(self, var_node):
        raise NotImplementedError()

    def visit_block_node(self, block_node):
        raise NotImplementedError()


    def visit_program_node(self, program_node):
        raise NotImplementedError()

    def visit_type_node(self, type_node):
        raise NotImplementedError()

    def visit_literal_node(self, literal_node):
        raise NotImplementedError()


    ''' - comment from PrintNodesVisitor
    visit_general(self, attrNode, nodeTypeStr, isParentNode)
    A general function for visiting nodes
    attrNode - the paramater is the attribute of the node instead of the node being visited
        The attribute is either a child (if non-terminal) or data (if terminal).
    nodeNameStr - the name of the node. Should start with a capital letter
    isParentNode - boolean to decide if terminal or non-terminal
    '''
    def visit_general(self, attrNode, nodeNameStr: str, isParentNode_Value_Cmd: str):
        pass # generic function so may not implement
        
    def visit_bool_node(self, bool_node):
        raise NotImplementedError()
    def visit_float_node(self, float_node):
        raise NotImplementedError()
    def visit_colour_node(self, colour_node):
        raise NotImplementedError()
    def visit_padwidth_node(self):
        raise NotImplementedError()
    def visit_padheight_node(self):
        raise NotImplementedError()

    def visit_padread_node(self, padread_node):
        raise NotImplementedError()

    def visit_padrandi_node(self, node):
        raise NotImplementedError()

    def visit_identifierarray_node(self, identifierarray_node):
        raise NotImplementedError()

    def visit_multiop_node(self, multiop_node):
        raise NotImplementedError()
    def visit_addop_node(self, addop_node):
        raise NotImplementedError()
    def visit_relop_node(self, addop_node):
        raise NotImplementedError()

    def visit_oneListAttribute(self, childNodes, nodeName:str):
        pass # generic function
    def visit_actualparams_node(self, acParams_node):
        raise NotImplementedError()
        
    def visit_functioncall_node(self, functioncall_node):
        raise NotImplementedError()

    def visit_subexpr_node(self, subexpr_node):
        raise NotImplementedError()

    def visit_unary_node(self, node):
        raise NotImplementedError()

    def visit_factor_node(self, node):
        raise NotImplementedError()

    def visit_term_node(self, node):
        raise NotImplementedError()
    def visit_simpleexpr_node(self, node):
        raise NotImplementedError()

    def visit_expr_node(self, node):
        raise NotImplementedError()

    def visit_variabledecl_node(self, variabledecl_node):
        raise NotImplementedError()

    def visit_variabledeclsuffix_node(self, node):
        raise NotImplementedError()

    def visit_variabledeclarray_node(self, node):
        raise NotImplementedError()

    def visit_requireexpr_node(self, node, nodeName):
        pass # generic function so may not be implemented

    def visit_printstatement_node(self, node):
        raise NotImplementedError()
    def visit_delaystatement_node(self, node):
        raise NotImplementedError()

    def visit_writestatement_node(self, node):
        raise NotImplementedError()

    def visit_rtrnstatement_node(self, node):
        raise NotImplementedError()

    def visit_ifstatement_node(self, node):
        raise NotImplementedError()
        
    def visit_forstatement_node(self, node):
        raise NotImplementedError()

    def visit_whilestatement_node(self, node):
        raise NotImplementedError()

    def visit_formalparam_node(self, node):
        raise NotImplementedError()

    def visit_formalparams_node(self, node):
        raise NotImplementedError()

    def visit_functiondecl_node(self, node):
        raise NotImplementedError()

