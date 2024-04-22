from parserblock import Parser

class SymbolTableChecker:
    def checkType(typeLiteral):
        return (typeLiteral == 'float') | (typeLiteral=='int') | (typeLiteral == 'bool') | (typeLiteral == 'colour')

class SymbolTable:

    def __init__(self):
        self.frames = [] # remember that you have a frame for each scope
        self.currentFrame = {}

    def push(self):
        self.frames.append(currentFrame)
        self.currentFrame = {} # reinitialise

    def insert(self, name:str, typeSymbol, value):
        self.currentFrame[name] = {
            'type': typeSymbol, # check type before update with possible error to user
            'value': value
        }

    def lookup(self, name):
        try:
            return self.currentFrame.get(name).get('value')
        except AttributeError:
            pass
        # need to use lookup in frames other than the current frame too

    # pop() - list has pop() function already

    def update(self, name:str, value):
        if name in self.currentFrame:
            self.currentFrame[name]['value'] = value # check type before update with possible error to user
        else:
            raise ValueError(f"Symbol '{name}' not found in the symbol table.")

    # def delete(self, name:str): # delete operation on a symbol is not used. Pop operation is done on a frame when end of repsective scope is reached.
    #     if name in self.symbols:
    #         del self.symbols[name]
    #     else:
    #         raise ValueError(f"Symbol '{name}' not found in the symbol table.")

    def __str__(self):
        strOut = ""
        for frameIndex in range(len(self.frames)):
            strOut += f'Frame {frameIndex}:'
            for symbolName in self.frames[frameIndex].keys():
                strOut += f'\n\t{symbolName}:'
                + f'\n\t\tType - {self.frames[frameIndex][symbolName]["type"]}'
                + f'\n\t\tValue - {self.frames[frameIndex][symbolName]["value"]}'
                
        return strOut

# Example usage:
if __name__ == "__main__":
    symbol_table = SymbolTable()

    # Insert symbols
    symbol_table.insert("x", 'int', 10)
    symbol_table.insert("y", 'int', 20)

    # Lookup symbols
    print("Value of 'x':", symbol_table.lookup("x"))  # Output: 10
    print("Value of 'z':", symbol_table.lookup("z"))  # Output: None

    # Update symbols
    symbol_table.update("x", 100)
    print("Updated value of 'x':", symbol_table.lookup("x"))  # Output: 100

    # # Delete symbols
    # symbol_table.delete("y")
    # print("After deleting 'y':", symbol_table)  # Output: {'x': 100}