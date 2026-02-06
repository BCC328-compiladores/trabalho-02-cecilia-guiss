// Exemplo 3: Operações com Arranjos
func reverse(arr : int[], size : int) : int [] {
    let result : int[] = new int[size];
    let i : int = 0;
    while (i < size) {
        result[i] = arr[size - i - 1];
        i = i + 1;
    }
    return result;
}

func main3() : void {
    let original : int[5] = [1, 2, 3, 4, 5];
    let reversed : int[] = reverse(original, 5);
    let j : int = 0;
    while (j < 5) {
        print(reversed[j]);
        j = j + 1;
    }
}
