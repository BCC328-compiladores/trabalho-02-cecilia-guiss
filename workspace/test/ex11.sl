// Exemplo 11: Funções Aninhadas para closure conversion

func make_adder(x: int) : (int) -> int {
    func adder(y: int) : int {
        return x +y ;
    }
    return adder;
}


func main() : void {
    let add_5 = make_adder(5);
    let result = add_5(3); // retorna 8
    print(result);
 
}