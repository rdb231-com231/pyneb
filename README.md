# PyNeb - Interpretador da Linguagem Nebula

**PyNeb** é o interpretador _oficial_ para a linguagem de programação **Nebula**, uma linguagem **moderna** desenvolvida para ser **acessível** e **expressiva** com `sintaxe` em português.

## Características da Linguagem Nebula

### Tipos de Dados Suportados
- Números inteiros e decimais (`número`)
- Textos (`string`)
- Listas (`lista`)
- Tuplas (`tupla`)
- Dicionários (`dicionário`)
- Conjuntos (`mesa`)
- Booleanos (`verdadeiro`/`falso`)
- Valor nulo (`nulo`)
- Funções (`função`)
- Classes (`classe`)

### Estruturas de Controle
```neb
se condição então
    # código
mas outra_condição então
    # código
senão
    # código
fim
```
### Loops
```
neb
caminhando i de 1 até 10 mudando 2 faça
    falar(i)
fim

para cada elemento em lista faça
    falar(elemento)
fim

enquanto condição faça
    # código
fim
```
### Funções
```neb
função saudação usando (nome, idade)
    retornar "Olá " + nome + ", você tem " + idade + " anos!"
fim
```
### Tratamento de Erros
```neb
tentar
    # código perigoso
caso erro faça
    falar("Ocorreu um erro: " + erro)
fim
```
### Programação de Orientação de Objetos
```neb
classe Pessoa
    declarar nome
    declarar idade
    
    função principal usando (nome, idade)
        esse.nome = nome
        esse.idade = idade
    fim
    
    função apresentar
        falar("Meu nome é " + esse.nome)
    fim
fim
```

# Funcionalidades do PyNeb
> Recursos **Principais**.

* Interpretador completo da linguagem Nebula

* Tratamento de erros detalhado

* Sistema de módulos e importação

* Shell interativo

### Funções Incorporadas
| Função	| Descrição |
| --- | --- |
| `falar(msg -> qualquer)`	| Exibe mensagens no console |
| `perguntar(msg -> qualquer)`	| Obtém entrada do usuário
| `número(val -> qualquer)`	| Converte para tipo numérico
`string(val -> qualquer)`	| Converte para texto
`lista(elementos... -> qualquer)`	| Cria/verifica listas
`tipo(val -> qualquer)`	| Retorna o tipo de um valor
`aleatório(min, max)`	| Gera números aleatórios
`esperar(segundos = 1)`	| Pausa a execução
`clicommand(comando -> string)`	| Executa comandos do sistema
| **E MUUUITO MAIS!** | São várias funções *`built-in`*!

# Executando Programas Nebula
### Via linha de comando:

```bash
nebula arquivo.neb
```

### Shell Interativo:

```bash
python pyneb.py
>>> falar("Olá Mundo!")
Olá Mundo!
```
## Exemplos de Código
### Fibonacci:
```neb
função fibonacci usando (n)
    se n <= 1 então
        retornar n
    senão
        retornar fibonacci(n-1) + fibonacci(n-2)
    fim
fim

falar(fibonacci(10))  # Exibe 55
```
### Manipulação de Arquivos:
```neb
conteúdo = ler_arquivo("dados.txt")
novo_conteúdo = conteúdo + "\nNova linha"
escrever_arquivo("dados_modificados.txt", novo_conteúdo)
```

Para descobrir como instalar **Nebula**, confira o arquivo `INSTALLING.md`.
