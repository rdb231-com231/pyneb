import sys
import os
import pyneb

if not sys.argv[1]:
    print("Please provide a file name as an argument.")
    sys.exit(1)

while True:

    if sys.argv[1] == '__shell__':
        text = input('nebula > ')
        if text == 'exit':
            break
        
        elif text == '':
            continue

        elif text == 'help':
            print("Commands:\n  help: Show this message\n  exit: Exit the shell\n  <code>: Run code\n .config: Show default config\n  .version: Show version\n  __path__: Show path to nebula.bat\n  __shell__: Enter shell mode")
            continue

        result, error, ctx = pyneb.run('<stdin>', text)

        if error: print(error.as_string())
        elif result: print(repr(result))
    
    elif sys.argv[1] == '.config':
        print(pyneb.DEFAULT_CONFIG)
        break
    
    elif sys.argv[1] == '.version':
        print(pyneb.DEFAULT_CONFIG['version'])
        break

    elif sys.argv[1] == '__path__':
        print(' ')
        print(os.path.abspath(sys.path[0] + '\\nebula.bat'))
        break
    

    else:
        if not sys.argv[1].endswith('.neb'):
            sys.argv[1] += '.neb'
        if not sys.argv[1].startswith('../'):
            sys.argv[1] = '../' + sys.argv[1]
        
        try:
            with open(sys.argv[1], 'r', encoding='utf-8') as f:
                text = f.read()
                _, error, ctx = pyneb.run(sys.argv[1], text)
                if error: 
                    print(error.as_string())
                    break
                else: break
        except FileNotFoundError:
            print(f"File {sys.argv[1]} not found. Please use command: \"nebula <filename>\"")
            break
