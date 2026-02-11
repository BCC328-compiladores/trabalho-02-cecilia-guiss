// Implementação do QuickSort em SL
// Nota: Como o interpretador usa semântica de valor para arrays, 
// as funções devem retornar o array modificado para propagar as alterações.

struct PartitionResult {
    pi : int;
    arr : int[];
}

func partition(arr : int[], low : int, high : int) : PartitionResult {
    let pivot : int = arr[high];
    let i : int = low - 1;
    let j : int = low;
    
    while (j < high) {
        if (arr[j] < pivot) {
            i = i + 1;
            // Troca manual (swap)
            let temp : int = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
        }
        j = j + 1;
    }
    
    let temp2 : int = arr[i + 1];
    arr[i + 1] = arr[high];
    arr[high] = temp2;
    
    return PartitionResult{i + 1, arr};
}

func quicksort(arr : int[], low : int, high : int) : int[] {
    if (low < high) {
        let res : PartitionResult = partition(arr, low, high);
        let pi : int = res.pi;
        arr = res.arr;
        
        // Ordena recursivamente as metades e atualiza a referência local do array
        arr = quicksort(arr, low, pi - 1);
        arr = quicksort(arr, pi + 1, high);
    }
    return arr;
}

func main() : void {
    let arr : int[] = [64, 34, 25, 12, 22, 11, 90, 5, 45, 1];
    let n : int = arr.size;

    print("Array original:");
    let i : int = 0;
    while (i < n) {
        print(arr[i]);
        i = i + 1;
    }

    // O QuickSort retorna o array ordenado
    arr = quicksort(arr, 0, n - 1);

    print("Array ordenado:");
    let k : int = 0;
    while (k < n) {
        print(arr[k]);
        k = k + 1;
    }
}
