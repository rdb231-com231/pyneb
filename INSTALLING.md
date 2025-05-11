# Instalando PyNeb

- [Download PyNeb Orion 1.3.2](https://github.com/rdb231-com231/pyneb/releases/tag/orion-1.0.0-alpha)

- [Extensão Download VSNeb](https://github.com/rdb231-com231/vsnebula/releases/tag/Base)

## PyNeb Orion 1.3.2
Vá para o **Download PyNeb Orion 1.3.2** e baixe o `Source Code`/Código Fonte disponibilizado. Crie uma pasta chamada `NebulaTests` (ou qualquer nome que quiser) para que possa colocar o `.zip` extraido **dentro**.

> Isso permitira que criemos códigos `Nebula` e possamos usar seu Interpretador ser muito esforço.

Abra a pasta que colocou o código fonte **extraído** com sua IDE (no caso desse tutorial, o VS Code) e crie um arquivo chamado `teste.neb`. Mas não vamos começar a escrever ***`ainda!`***.

## Instalando a Extensão VSNeb
Vá para **Extensão Download VSNeb** acima e baixe o arquivo `.vsix` ***apenas***, coloque em um lugar fácil de acessar como os **Downloads**. Após, abra o Visual Studio Code e na aba de `extensões`, clique nos 3 pontinhos acima e depois em `Install from VSIX...` ou `Instalar de VSIX...`. Então, selecione o arquivo `.vsix` baixado e reinicie o VS Code!

## Seu Primeiro Código Nebula
Abra novamente a pasta em que salvou o seu interpretador. Abra o arquivo `teste.neb` e aperte `Ctrl+'` para abrir o terminal. Clique na setinha (`v`) perto do `+` no canto superior direito do terminal e clique em `Command Prompt`, depois, use: 
```
cd <pasta em que salvou o PyNeb>
```

Agora, vamos conferir a instalação do seu ***`PyNeb!`***.

```
nebula .version
```
> Caso esteja usando Powershell, use: `./nebula .version`, isso acontece pois o PowerShell bloqueia comandos externos ***`.bat`*** por configuração de fábrica.

Se isso mostrar uma versão como **`1.3.2`**, sua instalação ocorreu corretamente. Podemos agora usar `PyNeb`.

Escreva em seu arquivo `teste.neb`.

```neb
falar("Olá, Mundo!")
```

Agora, ainda com seu terminal dentro da pasta aonde instalou `PyNeb`, use:
```
nebula teste
```

Divirta-se escrevendo códigos `PyNeb!`

> Autor: MewPlush

> Versão: 1.3.2.
