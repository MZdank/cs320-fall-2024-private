module ListSet = struct
    type t = int list

    let empty = []

    let singleton n = [n]

    (*recursively search*)
    let rec mem x s = 
        match s with
        | [] -> false
        | h::t -> 
            if x = h then
                true
            else
                mem x t

    (*Probably should do this tail recursive but whatever*)
    let card s =
        let rec length lst = 
            match lst with
            | [] -> 0
            | _::t -> 1 + (length t)
        in
        length s
    
    
    let union s1 s2 =
        let lst1 = List.sort compare s1 in
        let lst2 = List.sort compare s2 in
        let rec combine lst1 lst2 =
            (*If one list is empty, return other*)
            if lst1 = [] then
                lst2
            else if lst2 = [] then
                lst1
            else
                (*This is the same as head and tail
                but head and tail aren't in class lib*)
                let h1 = List.nth lst1 0 in
                let h2 = List.nth lst2 0 in
                let t1 = List.drop 1 lst1 in
                let t2 = List.drop 1 lst2 in
                if h1 < h2 then 
                    h1 :: combine t1 lst2
                else if h1 > h2 then
                    h2 :: combine lst1 t2
                else (*duplicates*)
                    h1 :: combine t1 t2
        in combine lst1 lst2

end

type set_info = {
    ind : int -> bool;
    mn : int;
    mx : int
}

module FuncSet = struct

    type t = set_info

    let empty = {
        ind = (fun _ -> false);
        mn = 2;
        mx = 1; 
    }

    let singleton n = {
        ind = (fun x -> x = n);
        mn = n;
        mx = n;
    }

    let mem x s = s.ind x

    let card s =
        let rec length i =
            (*If we get below mid at bottom*)
            if i < s.mn then 0
            else if s.ind i = true then   
                (*if value is in s add 1 to length*)
                1 + length (i - 1)
                (*Searching every val between max and min*)
            else length (i - 1)  
        in 
        length s.mx

    let union s1 s2 =
        let new_ind x = 
            (*if true in one of them, true in combine*)
            if s1.ind x = true || s2.ind x = true then
                true
            else
                false
        in
        let new_mn =
            if s1.mn < s2.mn then 
                s1.mn
            else s2.mn
        in
        let new_mx = 
            if s1.mx > s2.mx then
                s1.mx
            else s2.mx
        in
        {
            ind = new_ind;
            mn = new_mn;
            mx = new_mx
        }
end