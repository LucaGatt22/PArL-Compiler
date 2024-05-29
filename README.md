# Documentation PaRL Compiler for CPS2000
The compiler is split into the lexer, astnodes and parser and visitors to print nodes, for semantic analysis and for code generation of instructions understood by the `Pardis` VM. Hence, this compiler uses the Visitor Design Pattern.
 - `lexer` module:
   - `Lexer` class: has character categories and using them, identifies the tokens of the code string provided.
 - `astnodes` module: contains all the `ASTNode` classes (eg `ASTPrintStatementNode`) for the `ParL` language which may or may not be used for the AST generation.
 - `parser` module:
   - `Parser` class: parses the tokens created by the lexer and creates the AST node objects with the tokens provided by the lexer.
 - Visitors - these have visit node functions for all `ASTNode` classes:
   - `PrintNodesVisitor` - displays an AST (Abstract Syntax Tree) structure of nodes in the console as a string
   - `SemanticAnalysisVisitor` - performs type checking and scope checking (compares types and validates)
   - `CodeGenerationVisitor` - writes or generates `PArIR` instructions of code to a file, upon which these instructions should be copied in the VM input.

There is no website link to the VM in the GitHub repository, as it is not owned by me. The code part of the project stops after code generation (Task 4) and implementing array functionality (Task 5). The `PArIR` instructions are copied from the instructions file to the VM input. The same goes for the Assignment Question (mentioned below), which is owned by the lecturer under the University of Malta.

Note that throughout the documentation, the classes may have a small difference in their name, when compared to the codebase, such as `SemanticAnalysis` and `SemanticAnalysisVisitor`.
Check out the **Classes** section for more details about the mentioned classes.

Every Python file is constructed closely to the **e-BNF** provided in the assginment question.
`letter`, `digit` and `hex` are the character categories handled by `Lexer` but not by `Parser`.

## Classes
**Lexer**
 - creates tokens with the token types declared in the TokenType class of `lexerassignments.py`.
 - The lexer produces tokens such as identifiers, keywords, values and operators.
 - Comments are allowed. The lexer removes any comments, before the lexical analysis is started. These are recognised by the // and /* ... */ symbols.

**Parser**
 - generates AST (Abstract Syntax Tree)
 - The parser parses the tokens which are created for the eBNF statements Type (ASTTypeNode) onwards (excluding the identifier), and tokens representing characters such as the semicolon and the colon.
astnodes - The structure of the AST is determined ASTNode classes in `astnodes.py`. Each AST node class has an:
 - __init__() function to hold the name of the class and its children (other AST nodes)
   - some classes require two __init__() functions to implement the option shown by the eBNF statement
 - function such as addLiteral() or addTerm() which adds a node to a list of nodes found in the class, as sometimes a child of a node is a list of nodes (instead of a node).
 - accept() function which links to the corresponding visit_node function.

**PrintNodesVisitor** - traverses through AST and prints the details of the nodes to have a visual of the tree.

**SemanticAnalysis** - This is a visitor, meaning it traverses through AST, this further validating program through type and scope checking.
 - Each AST node has a corresponding visit_node() function. For example, an assignment node has the visit_assignment_node().
 - An example of type check: visit_assignment_node checks that the expression type corresponds with the variable type as stored in the symbol table.
 - To be able to do type checking for statements, the type of each expression is returned from the visit function, until the point of type check in a parent visit function is reached.

**SymbolTable** - Now that the symbol table is mentioned, the symbol table interacts with both the SemanticAnalysisVisitor and the CodeGenerationVisitor.
 - The symbol table is split into frames, each frame containing a dictionary/mapping between symbol names and their type and value. This is so that the symbol table keeps record of all the symbols created grouped by the scope in which they were created.
 - The symbol table is a stack of frames. This means that LIFO structure is implemented. At the start of a new scope, an empty frame is pushed on the top of the stack. Similarly, at the end of a scope, the current frame is popped off the stack.
 - The implemented symbol table is still has a stack of frames, but has also the currentFrame. The current frame is not found in the stack. Opening a scope means adding the current frame to the top of the stack and emptying `currentFrame` for the new scope. Similarly, popping means removing the removing the top frame in the stack and placing it in `currentFrame` while discarding its old contents.
 - Although the symbol table has the stack functions (push and pop), it has also lookup functions (in current frame and in all frames), an insert() and an update() function.

**CodeGenerationVisitor** - It generates PArIR instrucions based on the PArL program given. **Not implemented yet.**
 - Address a location in the stack using [symbolIndex:frameIndex]
 - Started with one-by-one inputting of transactions in the generated code file, changed to a global instructions list and changed again to having each visit_node() function return its instructions to the parent node. This is done to be able to do function declaration and if statements. For example, to determine the number of lines to jump the if block, the instructions of the if block must be returned before generating the instructions, while keeping the overall correct chronology of instructions. By chronology, it is meant that 'cjmp' must still come before the if block.
   - By returning instructions instead of writing immediately to the file, it allows to get information about blocks before writing them in the file. Using this method, writing to file (using a new function to segregate functionality) is the last thing after the AST is traversed. Either the program node, since it is the root, writes to the file, or the driver code of the code generation module. This is done by calling this function. 

## More points on semantic SemanticAnalysis
WriteStatement
 - `__write` in PArL takes 3 expr i.e. colour, pointX, pointY
 - `__write_box` in PArL takes 5 expr i.e. colour, topLeftPointX, topLeftPointY, bottomRightPointX, bottomRightPointY

Function
 - signature - A function signature is introduced to specify the function well to type check it.
 - return - A function must have the abilty to return an expression - not tested
 - No jumps are required to go from a function to another since this is done by `call` and `ret`.

Unused PArL and VM commands
 - The 'min' and 'max' VM commands do not need to be used.
 - '__clear' does not exist in PaRL's eBNF, so I cannot include the use of the VM's 'clear' command.
 - Since I can restrict my language to work only with +ve integers, the unary operator '-' is allowed by the Lexer and Parser but disregarded in code generation (CodeGenerationVisitor).

〈VariableDeclSuffix〉in eBNF is wrong. It should have brackets before `=` and after `〈VariableDeclArray〉` for clarity.
 - Arranged 〈VariableDeclSuffix〉 to suit the example `let list_of_integers : int[] = [23, 54, 3, 65, 99, 120, 34, 21];`

Order (BIDMAS) should be accepted, although not tested due to the structure of the eBNF given, where there are:
 - different kinds of operators in the eBNF such as relationalOperator (relationalOp)
 - `()` via the `SubExpr` node and non-terminal
