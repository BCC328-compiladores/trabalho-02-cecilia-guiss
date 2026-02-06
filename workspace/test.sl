func factorial(n : int) : int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

func main() : void {
    let result : int = factorial(5);
    print(result);  // Deve imprimir 120
    let s : string = "Hello";
    let f : float = 12.5;
}
