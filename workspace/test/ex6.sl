// Exemplo 6: função map
forall a b . func map (f : (a) -> b, v : a[]) : b[] {
    let result = new b[v.size];
    for (i = 0; i < v.size ; i++) { 
      result[i] = f(v[i]);
    }
    return result;
}

func double(x: int): int {
    return x * 2;
}

func main(): void {
    let nums: int[] = [1, 2, 3, 4, 5];
    let doubled: int[] = map(double, nums);
    
    for (let i : int = 0; i < doubled.size; i++) {
        print(doubled[i]);
    }
}


