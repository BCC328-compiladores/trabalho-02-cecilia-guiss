func f(x : int) : int {
    return x+1;
    x++;
}

func main() {
    let x : int = 1;
    x = f(x);
}
