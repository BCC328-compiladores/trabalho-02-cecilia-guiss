// Exemplo 10: matrizes e variaveis globais

let g: int = 11;
let h: int = 11+g;

func main() : void {
    let nums: int[][] = [[1, 2, 3, 4, 5],[5, 4, 3, 2, 1],[6, 7, 8, 9, 1]];

    for (let i : int = 0; i < 5; i++) {
        for (let j : int = 0; j < 3; j++){
            print(nums[i][j]);
            print(h)
        }
    }
}