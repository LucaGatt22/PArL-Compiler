import codegeneration

def read_file_as_string(file_path):
    try:
        with open(file_path, 'r') as file:
            file_content = file.read()
        return file_content
    except FileNotFoundError:
        print(f"File '{file_path}' not found.")
        return None
    except Exception as e:
        print(f"Error reading file: {e}")
        return None


file_content = read_file_as_string("code.parl")
if file_content is None: exit() # user would have been notified of error already
codegeneration.driverCode(file_content)
