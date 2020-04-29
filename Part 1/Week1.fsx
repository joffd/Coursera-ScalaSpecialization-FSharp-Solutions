open System

/// Exercise 1

// Calc the value on the Pascal's triangle
// using recursion
// c: column, r: row


let rec pascal (col: int) (row: int) =
    match (col, col = row) with
    | (0, _) -> 1
    | (_, true) -> 1
    | (_, _) -> pascal (col - 1) (row - 1) + pascal (col) (row - 1)

// Let's print it to check
pascal 5 7
[0..10]
|> List.map (fun row ->
    [0..row]
    |> List.iter (fun col -> (printf "%i " (pascal col row)))
    Console.WriteLine ""
    )

/// Exercise 2

// Check parenthese balancing
// OK -> "Hello (Hi) !"
// OK -> "Hi!"
// Not OK -> ":-)"
// Not OK -> "())("

let checkParenthesesBalancing (chars: list<char>) : bool =
    
    let rec check (count: int) (list: list<char>) =
        match (list, count) with
        | ([], 0) -> true
        | ([], _) -> false
        | (h::t, 0) ->
            match h with
            | '(' -> check 1 t
            | ')' -> false
            | _   -> check 0 t
        | (h::t, c) ->
            match h with
            | '(' -> false
            | ')' -> check 0 t
            | _   -> check c t
    check 0 chars

let testEx2 (str: string) =
    str
    |> Seq.toList
    |> checkParenthesesBalancing

["Hello" ; "" ; "Hi (test)" ; "Test (" ; "()" ; "())("]
|> List.map testEx2


/// Exercise 3

// Count the number of way to give back change
// given an amount (int) and a list of coins
// For example, there are 3 ways to give change for 4 
// if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
// Coins can be reused

let countChange (money: int) (coins: list<int>) : int =
    let rec count (counter: int) (sum: int) (remCoins: list<int>) =
        match sum with
        | 0 -> 1
        | r when r < 0 -> 0
        | r -> 
            match remCoins with
            | [] -> 0
            | h::t -> (count counter (r - h) remCoins) + (count counter r t) 
    count 0 money coins

let listCoints = [1;2;3]
countChange 5 listCoints
