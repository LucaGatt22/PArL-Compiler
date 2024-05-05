class SymbolTable:

    def __init__(self):
        self.frames = [] # remember that you have a frame for each scope
        self.currentFrame = {}

    def push(self):
        self.frames.append(currentFrame)
        self.currentFrame = {} # reinitialise

    def insert(self, name:str, typeSymbol, value=None):
        self.currentFrame[name] = {
            'name': name,
            'type': typeSymbol, # check type before update with possible error to user
            'valueAddr': { # '[{len(currentFrame)}:{len(frames)}]' # symbolIndex:frameIndex
                'symbolIndex': len(currentFrame)
                'frameIndex': len(frames)
            }
        } # introduced a new type 'function' for identifiers of functions. Introduced function/method signature

    def lookupCurrentFrame(self, name):
        return self.currentFrame.get(name)
        # need to use lookup in frames other than the current frame too
    def lookup(self, name):
        symbol = self.lookupCurrentFrame(name)
        if symbol != None: return symbol
        for frame in reversed(self.frames):
            if name in frame:
                return frame[name]
        if symbol == None: raise Exception(f'Symbol {name} does not exist')
    def lookupGetType(self, name):
        symbol = self.lookup(name)
        return symbol.get('type')
    def lookupGetValueAddr(self, name:str):
        symbol = self.lookup(name)
        return symbol['valueAddr']

    # pop() - list has pop() function already
    def pop(self):
        if self.frames:
            self.currentFrame = self.frames.pop()
        else:
            raise IndexError("Cannot pop from an empty symbol table.")

    '''
    update() - It changes/updates a symbol's value
    @return void as updates symbol's value in the function
    '''
    def update(self, name:str, value):
        found = False
        for i in range(len(self.frames) - 1, -1, -1):
            if name in self.frames[i]:
                self.frames[i][name]['value'] = value  # check type before update with possible error to user
                found = True
                break
        if not found:
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
