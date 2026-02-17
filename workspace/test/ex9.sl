forall a b . func map(f : (a) -> b, v : a[]) : b[] {
    let result = new b[v.size];
    for (i = 0; i < v.size ; i++) { 
        result[i] = f(v[i]);
    }
    return result;
}

func main(): void {
    let nums: int[] = [1, 2, 3, 4, 5];
    let doubled: int[] = map(\x : int :: int -> x * 2, nums);
    
    for (let i : int = 0; i < doubled.size; i++) {
        print(doubled[i]);
    }
    tripled_func = \x : int :: int -> x * 3;
    let tripled: int[] = map(tripled_func, nums);
    
    for (let i : int = 0; i < doubled.size; i++) {
        print(doubled[i]);
    }
}
