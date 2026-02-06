[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/X15MPpfH)
# Trabalho prático de BCC328 - Construção de Compiladores I

## Instruções para compilar o relatório

dentro do diretório `relatorio/`, utilize `make relatorio` para compilar e gerar o PDF.


## Instruções para executar o projeto

- Iniciando container

```
docker-compose up -d
docker-compose exec sl bash
```
## Como Executar

Utilize o `Makefile` para compilar e rodar os testes.

### Comandos Disponíveis

*   **`make build`**: Compila o projeto.
*   **`make test`**: Executa todos os testes (Lexer, Parser, Pretty Printer) para todos os exemplos (`test/ex1.sl` a `test/ex7.sl`).

### Rodando Testes Específicos

Você pode rodar os estágios individualmente:

*   **`make lexer`**: Roda o analisador léxico.
*   **`make parser`**: Roda o analisador sintático (imprime a AST).
*   **`make pretty`**: Roda o pretty printer (formata o código).

### Testando Arquivos Individuais

Para testar apenas um arquivo específico, passe o caminho do arquivo como argumento após o comando:

```bash
make lexer test/ex1.sl
make parser test/ex3.sl
make pretty test/ex7.sl
```

Se nenhum arquivo for especificado, o comando rodará para todos os exemplos padrão do diretório `test/`.

