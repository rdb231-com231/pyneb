import sys
import os
import pyneb
from termcolor import colored, cprint

try:
    if not sys.argv[1]:
        print("Please provide a file name as an argument.")
        sys.exit(1)
except IndexError:
    print("Please provide a file name as an argument.")
    sys.exit(1)

while True:
    cprint("\nLembre-se: nebula.bat pode não identificar seu python corretamente! Tente trocar Python3 por Python ou só py antes de re-instalar qualquer coisa!\n", 'red', attrs=['bold'])
    if sys.argv[1] == '__shell__':
        cprint("Nebula Shell", 'blue', attrs=["bold", "underline", "blink"])
        print('\n')
        cprint("Enter 'exit' to exit the shell.", 'green')
        cprint("Enter 'help' for help.", 'green')
        cprint("Made by MewPlush, Brazil.", 'light_red', attrs=['bold', 'underline'])
        print('\n\n')
        while True:
            text = input(colored('nebula >> ', 'magenta'))
            if text == 'exit':
                break
            
            elif text == '':
                continue

            elif text == 'help':
                print("Commands:\n  help: Show this message\n  exit: Exit the shell\n  <code>: Run code\n .config: Show default config\n  .version: Show version\n  __path__: Show path to nebula.bat\n  __shell__: Enter shell mode")
                continue

            result, error, ctx = pyneb.run('<stdin>', text)

            if error: cprint(error.as_string(), 'red', attrs=['dark', 'bold']); break
            
            if result: cprint(repr(result), 'light_yellow', attrs=['bold']); continue
        break
    
    elif sys.argv[1] == 'setmain':
        with open('.config', 'w', encoding='utf-8') as f:
            if not sys.argv[2].endswith('.neb'):
                sys.argv[2] += '.neb'
            if not sys.argv[2].startswith('../'):
                sys.argv[2] = '../' + sys.argv[2]
            f.write(f"access_point_main={sys.argv[2]};")
            f.write(f"\nversion={pyneb.DEFAULT_CONFIG['version']};")
            f.write(f"\ninno_setup={"'true'" if os.path.exists(os.path.abspath(os.path.join(sys.path[0], "unins000.exe"))) else "'false'"};")
        cprint('Successo!', 'green', attrs=['bold'])
        break


    elif sys.argv[1] == 'iniciar' or sys.argv[1] == 'init' or sys.argv[1] == '--init':
        with open('.config', 'w', encoding='utf-8') as f:
            try:
                f.write(f"access_point_main={sys.argv[2] if sys.argv[2] else 'not-defined'};")
            except IndexError:
                f.write(f"access_point_main='not-defined';" )
            f.write(f"\nversion={pyneb.DEFAULT_CONFIG['version']};")
            f.write(f'\ninno_setup={"'true'" if os.path.exists(os.path.abspath(os.path.join(sys.path[0], "unins000.exe"))) else "'false'"};')
        cprint('Successo!', 'green', attrs=['bold'])
        break

    elif sys.argv[1] == '.':
        with open('.config', 'r', encoding='utf-8') as f:
            for i in f.read().split(';'):
                if i.startswith('access_point_main'):
                    filename = i.split('=')[1]
                    if filename == '\'not-defined\'':
                        cprint("Parece que um arquivo principal não foi definido, use nebula setmain <arquivo> para definir um arquivo principal.", 'yellow', attrs=['dark', 'bold']);
                        break
        if not filename.endswith('.neb'):
            filename += '.neb'
        if not filename.startswith('../'):
            filename = '../' + filename
        
        try:
            with open(filename, 'r', encoding='utf-8') as f:
                text = f.read()
                if not text: print("Arquivo vazio, favor adicionar algum código."); break
                _, error, ctx = pyneb.run(filename, text)
                if error: cprint(error.as_string(), 'red', attrs=['dark', 'bold']); break
        except FileNotFoundError:
            cprint(f"Arquivo {filename} não encontrado. Favor usar comando: \"nebula <filename>\" ou \"nebula setmain <filename>\"", 'cyan', attrs=['dark', 'bold'])
            break
    
    elif sys.argv[1] in ('--config', '-c'):
        print(pyneb.DEFAULT_CONFIG)
        break
    
    elif sys.argv[1] in ('--version', '-v'):
        cprint(pyneb.DEFAULT_CONFIG['version'], 'blue', attrs=['underline'])
        break

    elif sys.argv[1] in ('__path__', "--p"):
        print(' ')
        print(os.path.abspath(sys.path[0] + '\\nebula.bat'))
        break

    elif sys.argv[1] == 'special-keys':
        for k, v in pyneb.DEFAULT_CONFIG['special-keys'].items():
            if k == 'special-keys': continue
            print(f'{k}: {v}\n')
        break

    elif sys.argv[1] in pyneb.DEFAULT_CONFIG:
        print(pyneb.DEFAULT_CONFIG[sys.argv[1]])
        break
    

    else:
        if not sys.argv[1].endswith('.neb'):
            sys.argv[1] += '.neb'
        if not sys.argv[1].startswith('../') and not sys.argv[1].startswith('pyneb/src/'):
            sys.argv[1] = '../' + sys.argv[1]
        if sys.argv[1].startswith('pyneb/src/'):
            sys.argv[1] = sys.argv[1].replace('pyneb/src/', '')
        
        try:
            with open(sys.argv[1], 'r', encoding='utf-8') as f:
                text = f.read()
                if not text: print("Arquivo vazio, favor adicionar algum código."); break
                _, error, ctx = pyneb.run(sys.argv[1], text)
                if error: cprint(error.as_string(), 'red', attrs=['dark', 'bold']); break
        except FileNotFoundError:
            cprint(f"File {sys.argv[1]} not found. Please use command: \"nebula <filename>\"", 'cyan', attrs=['dark', 'bold'])
            break
    
    break
