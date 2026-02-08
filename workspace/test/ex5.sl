// Exemplo 5: função identidade
func id(x) {
    return x;
}

func main() : void {
    let value : int = 5;
    let identity = id(value);
    print(identity);
}