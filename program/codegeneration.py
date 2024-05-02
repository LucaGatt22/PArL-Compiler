from parserblock import Parser # parse, can output (like a log)
from symboltable import SymbolTable
from printnodesvisitor import PrintNodesVisitor
from astvisitor import ASTVisitor
from semanticanalysis import SemanticAnalysisVisitor

class CodeGenerationVisitor:
    