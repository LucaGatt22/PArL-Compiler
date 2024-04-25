from parserblock import Parser # parse, can output (like a log)
from symboltable import SymbolTable


class SemanticAnalyser:
    self.lexer = lex.Lexer()
    
    def checkType(self):
        if ()



if __name__ == '__main__':
    # parser driver code
    #parser = Parser("x=23;")
    parser = Parser("x=   23 ; y=  100; { z = 23 ;xy=3; } fun hello()->bool{return 2;} x=hello()+2*3/6-2*(8-4);")
##    parser = Parser("x = hello();")
##    parser.test = True # test
##    parser = Parser("x=   23 ; y=  100;")
##    parser = Parser('{ z = 23 ; xy=3; }')
    parser.Parse()

    

    # semantic analysis
    



    # visitor
    print_visitor = ast.PrintNodesVisitor()
    parser.ASTroot.accept(print_visitor)