func make_multiplier(factor: int) : (int) -> int {
    func multiplier(x: int) : int {
        return x * factor;
    }
    return multiplier;
}

func main() {
    let tripler = make_multiplier(3);
    let x = 10;
    
    func local_test(y: int) : int {
        // Captura 'tripler' e 'x'
        return tripler(y) + x;
    }
    
    let result = local_test(5); // (5 * 3) + 10 = 25
    print(result);
}
