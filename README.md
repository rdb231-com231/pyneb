# PyNeb (Python Nebula)

PyNeb é o principal `interpretador` para **Nebula**, uma linguagem de programação simples em português para iniciantes; Nebula é uma linguagem com funções variadas, incluindo: `váriaveis`, `expressões matemáticas`, `lógica`, `loop`, `funções`, `imports` e mais!

Confira alguns exemplos da linguagem ***Nebula!***:
```neb
    # math.neb
    função add usando (x, y)
        confira número(x) usando "InvalidSyntax" detalhes "Esperava-se um número"
        confira número(y) usando "InvalidSyntax" detalhes "Esperava-se um número"
        retornar x + y
    fim
```

```neb
    # main.neb
    importar add de "math"
    falar(add(2, 5))
    # 7
```

Para descobrir como instalar **Nebula**, confira o arquivo `INSTALLING.md`.
