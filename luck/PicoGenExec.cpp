(* To be preprocessed with gcc -E -P -o PicoGenExec.core *)
#define STACKLENGTH 10
#define MEMLENGTH 10
#define PROGLENGTH 10
#define RANGE 10
#define RUNLENGTH 10

#define MEM(n) [| n | 0 <= n && (n < MEMLENGTH {1} || {9} n < 3) |]
#define PROG(n) [| n | 0 <= n && n < PROGLENGTH |]

data Label = L | H

sig eqL :: Label -> Label -> Bool
fun eqL l1 l2 = 
    case (l1, l2) of 
      | (L, L) -> True
      | (H, H) -> True
      | _ -> False
    end 

sig isLow :: Label -> Bool
fun isLow l = eqL L l

sig isHigh :: Label -> Bool
fun isHigh l = eqL H l 

sig wellFormedLabel :: Label -> Bool
fun wellFormedLabel l = 
    case l of 
      | 3 % L -> True
      | 1 % H -> True
    end

sig labelLeq :: Label -> Label -> Bool
fun labelLeq l m =
  case l of
  | H -> case m of
         | H -> True
         | L -> False
         end
  | L -> True
  end

data Atom = Atom Label Int

data Instr = Noop 
           | Add 
           | Push Atom
           | Pop
           | Load 
           | Store 
           | Jump
           | Call Int Bool
           | Return Bool
           | Halt

sig isHighAtom :: Atom -> Bool
fun isHighAtom pc = 
    let' (Atom l _) = pc in 
    isHigh l

sig indistAtom :: Atom -> Atom -> Bool
fun indistAtom a1 a2 = 
    let' (Atom l1 v1) = a1 in
    let' (Atom l2 v2) = a2 in 
    eqL l1 l2 && if isLow l1 then v1 == v2 else (inRange v1 && inRange v2)

sig indistAtomList :: [Atom] -> [Atom] -> Bool
fun indistAtomList l1 l2 = 
    case (l1, l2) of 
      | ([], []) -> True
      | (a1:t1, a2:t2) -> indistAtom a1 a2 && indistAtomList t1 t2
      | _ -> False
    end

sig indistInstr :: Instr -> Instr -> Bool
fun indistInstr i1 i2 =
  case (i1, i2) of 
    | (Push a1, Push a2) -> indistAtom a1 a2
    | (Noop, Noop) -> True
    | (Add, Add) -> True
    | (Pop, Pop) -> True
    | (Load, Load) -> True
    | (Store, Store) -> True
    | (Jump, Jump) -> True
    | (Call a1 True, Call a2 True) -> inRange a1 && a1 == a2
    | (Return True, Return True) -> True
    | (Halt, Halt) -> True
    | _ -> False
  end

data StkElt = Data Atom
            | Ret  (Label, (Int, Bool))

sig indistStkElt :: StkElt -> StkElt -> Bool
fun indistStkElt s1 s2 = 
    case (s1, s2) of 
      | (Data d1, Data d2) -> indistAtom d1 d2
      | (Ret (l1, (i1, True)), Ret (l2, (i2, True))) ->
        case (l1, l2) of
        | (H, H) -> inRange i1 && inRange i2
        | (L, L) -> inRange i1 && i1 == i2
        | _ -> False
        end
      | _ -> False
    end

sig indistStkLow :: [StkElt] -> [StkElt] -> Bool
fun indistStkLow s1 s2 = 
    case (s1, s2) of 
      | ([], []) -> True
      | (x1 : s1', x2 : s2') -> indistStkElt x1 x2 && indistStkLow s1' s2'
      | _ -> False
    end

sig indistStkCrop2 :: [StkElt] -> [StkElt] -> Bool
fun indistStkCrop2 s1 s2 = 
    case s2 of 
      | Ret (l, (i, b)) : s2' -> 
        case l of 
          | L -> indistStkLow s1 (Ret (l, (i, b)) : s2')
          | H -> indistStkCrop2 s1 s2'
        end
      | _ : s2' -> indistStkCrop2 s1 s2'
      | [] -> indistStkLow s1 []
    end

sig indistStkCrop :: [StkElt] -> [StkElt] -> Bool
fun indistStkCrop s1 s2 = 
    case s1 of 
      | Ret (l, (i, b)) : s1' -> 
          case l of 
            | L -> indistStkCrop2 (Ret (l, (i, b)) : s1') s2 
            | H -> indistStkCrop s1' s2
          end
      | _ : s1' -> indistStkCrop s1' s2
      | [] -> indistStkCrop2 [] s2
    end

sig length :: [a] -> Int -> Bool
fun length l {n @i} = 
    if n == 0 then 
        case l of 
          | [] -> True
          | _  -> False
        end
    else case l of 
           | [] -> False
           | (x : xs) -> length xs (n-1)
    end

sig wellFormedMemory :: [Atom] -> Int -> Bool
fun wellFormedMemory l n = 
    if n == 0 then 
        case l of 
          | [] -> True
          | _  -> False
        end
    else case l of 
           | [] -> False
           | ((Atom lab x) : xs) -> inRange x && wellFormedLabel lab 
                                    && wellFormedMemory xs (n-1)
    end
    

sig stackLength :: [StkElt] -> Int -> Int 
fun stackLength stack {n @i}=
    case stack of 
      | [] -> n
      | (Ret _ : _ ) -> n
      | (Data _ : s) -> stackLength s (n+1)
    end

sig indistInstrList :: [Instr] -> [Instr] -> Bool
fun indistInstrList i1 i2 = 
    case (i1, i2) of 
      | ([], []) -> True
      | (h1:t1, h2:t2) -> indistInstr h1 h2 && indistInstrList t1 t2 
      | _ -> False
    end

data AS = AS [Atom] [Instr] [StkElt] Atom

sig inRange :: Int -> Bool
fun inRange x = [| x | 0 <= x && x < RANGE |]

fun wellFormedAtom a =
  let' Atom l n = a in
  inRange n && wellFormedLabel l

fun wellFormedMultiInstr i j stackSize stack =
  case i of
  | 1 % Push (Atom l n) ->
    wellFormedLabel l &&
      case j of
      | 1 % Load -> MEM(n) && True
      | 3 % Store -> MEM(n) && stackSize > 0
      | 1 % Jump -> PROG(n) && True
      | 1 % Call m True -> PROG(n) && [|m| 0 <= m && m < stackSize|]
      | _ -> False
      end
  | 0 % _ -> False
  end

sig wellFormedInstrs :: [Instr] -> Int -> Int -> [StkElt] -> Bool
fun wellFormedInstrs instrs {addr @i} stackSize stack = 
    case instrs of 
      | i:is -> if addr == 0 then
                  (wellFormedInstr i stackSize stack {10} || {6}
                   case is of
                   | [] -> False
                   | j : _ -> wellFormedMultiInstr i j stackSize stack
                   end)
                else wellFormedInstrs is (addr-1) stackSize stack
      | _ -> False
    end

data Maybe a = Just a | Nothing

sig nth :: Int -> [a] -> Maybe a
fun nth n l =
    if n == 0 then 
        case l of 
          | i:_ -> Just i
          | _ -> Nothing
        end
    else case l of 
           | _:is -> nth (n-1) is 
           | _ -> Nothing
         end

-- sig nthLoad :: Int -> [Atom] -> (Int,[Atom],[Instr], Label, [StkElt], Atom) -> Bool
-- fun nthLoad n l rest =
--     if n == 0 then 
--         case l of 
--           | Atom ldata d : _ -> 
--              let' (len, m, is, lptr, s', pc') = rest in
--              runsLong len (AS m is (Data (Atom (join lptr ldata) d) : s') pc')
--           | _ -> True
--         end
--     else case l of 
--            | _:is -> nthLoad (n-1) is rest
--            | _ -> True
--          end

sig putNth :: Int -> a -> [a] -> Maybe [a] 
fun putNth n x l = 
    if n == 0 then Just (x : l)
    else case l of 
           | h:t -> case putNth (n-1) x t of 
                      | Just l -> Just (h:l)
                      | _ -> Nothing
                    end
           | [] -> Nothing
         end

sig join :: Label -> Label -> Label
fun join l1 l2 = 
    case l1 of 
      | L -> l2 
      | H -> H
    end

sig add :: Atom -> Atom -> Atom
fun add a1 a2 = 
    let' (Atom l1 x1) = a1 in
    let' (Atom l2 x2) = a2 in 
#ifdef BUGARITH
    Atom L (x1 + x2)
#else
    Atom (join l1 l2) (x1 + x2)
#endif

sig wellFormedInstr :: Instr -> Int -> [StkElt] -> Bool
fun wellFormedInstr i stackSize stack = 
    case i of 
      | 1 % Noop -> True
      | 40 % Add  ->
          case stack of
          | Data _ : Data _ : _ -> True
          | _ -> False
          end
      | 100 % Push (Atom l n) -> inRange n && wellFormedLabel l
      | 40 % Pop  ->
#ifdef BUGPOP
          True
#else
          case stack of
          | Data _ : _ -> True
          | _ -> False
          end
#endif
      | 40 % Load ->
          case stack of 
          | (Data (Atom _ n):_) -> MEM(n)
          | _ -> False
          end
      | 40 % Store ->
          case stack of 
            | Data (Atom l n) : Data a : _ -> MEM(n)
            | _ -> False
          end
      | 40 % Jump ->
          case stack of
            | Data (Atom _ n) : _ -> PROG(n)
            | _ -> False
          end
      | 40 % Call n True -> [| n | 0 <= n && n < stackSize |]
      | Call _ False -> False
      | 40 % Return True ->
          case stack of 
            | (Data _ : Ret _ : _) -> True
            | _ -> False
          end
      | Return False -> False
      | 5 % Halt   -> True
    end

sig step :: Int -> AS -> Bool
fun step len st = 
    let' (AS m is s pc) = st in
    let' (Atom lab addr) = pc in
    case nth addr is of 
    | Just i -> 
      case i of 
      | Noop -> runsLong len (AS m is s (Atom lab (addr+1)))
      | Add  -> 
        case s of 
        | (Data a1:Data a2:s') -> 
          runsLong len (AS m is (Data (add a1 a2):s') (Atom lab (addr+1)))
        | _ -> True
        end
      | Push x ->
#ifdef BUGPUSH
        let' Atom l n = x in
        runsLong len (AS m is (Data (Atom L n):s) (Atom lab (addr+1)))
#else
        runsLong len (AS m is (Data x:s) (Atom lab (addr+1)))
#endif
      | Pop  -> 
        case s of -- wellFormedInstr checks that no data is popped
        | _ : s' -> runsLong len (AS m is s' (Atom lab (addr+1)))
        | _ -> True 
        end
      | Load ->
        case s of 
        | (Data a:s') -> 
          let' (Atom lptr ptr) = a in
          case nth ptr m of 
          | Just (Atom ldata d) -> 
              runsLong len
                (AS
                  m
                  is
#ifdef BUGLOAD
                  (Data (Atom ldata d):s')
#else
                  (Data (Atom (join lptr ldata) d):s')
#endif
                  (Atom lab (addr+1)))
          | _ -> True
          end  
--          nthLoad ptr m (len, m, is, lptr, s', (Atom lab (addr+1)))
        | _ -> True
        end
      | Store -> 
        case s of 
        | (Data (Atom lptr ptr):Data (Atom l n):s') -> 
          case nth ptr m of
          | Just (Atom l' n') ->
#ifndef BUGWDOWNHIGHPTR
            labelLeq l' lptr &&
#endif
#ifndef BUGWDOWNHIGHPC
            labelLeq lab lptr &&
#endif
            True
          | Nothing -> False
          end &&
#ifdef BUGSTOREVALUE
          case putNth ptr (Atom L n) m of
#elif defined(BUGSTOREPOINTER)
          case putNth ptr (Atom (join lab l) n) m of
#elif defined(BUGSTOREPC)
          case putNth ptr (Atom (join lptr l) n) m of
#else
          case putNth ptr (Atom (join (join lptr lab) l) n) m of 
#endif
          | Just m' -> runsLong len (AS m' is s' (Atom lab (addr+1)))
          | _ -> True
          end 
        | _ -> True
        end 
      | Jump -> 
        case s of 
        | (Data (Atom labPtr ptr):s') -> 
          runsLong len
            (AS
              m
              is
              s'
#ifdef BUGJUMPNORAISE
              (Atom lab ptr)
#elif defined(BUGJUMPLOWER)
              (Atom labPtr ptr)
#else
              (Atom (join lab labPtr) ptr)
#endif
            )
        | _ -> True
        end
      | Call n True -> 
        case s of 
        | (Data (Atom labPtr ptr):s') -> 
          case putNth n (Ret (lab, (addr+1, True))) s' of 
          | Just s'' -> runsLong len (AS m is s''
#ifdef BUGCALL
              (Atom labPtr ptr)
#else
              (Atom (join labPtr lab) ptr)
#endif
            )
          | _ -> True 
          end
        | _ -> True
        end
      | Call _ False -> False
      | Return True -> 
        case s of 
        | Data (Atom lx x) : Ret (retl, (retptr, True)) : s' ->
          runsLong len (AS m is
#ifdef BUGRETURN
            (Data (Atom lx x) : s')
#else
            (Data (Atom (join lx lab) x):s')
#endif
            (Atom retl retptr))
        | _ : Ret (_, (_, False)) : _ -> False
        | _ -> True
        end
      | Return False -> False
      | Halt -> True
      end
    | _ -> True
    end

sig runsLong :: Int -> AS -> Bool
fun runsLong len st = 
    if len <= 0 then True
    else 
      let' (AS m i s pc) = st in
      let' (Atom lab addr) = pc in 
      inRange addr && 
      wellFormedInstrs i addr (stackLength s 0) s &&
      step (len-1) (AS m i s (Atom lab addr))

sig wellFormedStack :: [StkElt] -> Int -> Bool
fun wellFormedStack st n = 
    if n == 0 then 
        case st of 
          | [] -> True
          | _  -> False
        end
    else case st of 
           | [] -> False
           | 5 % (Data (Atom l x) : xs) -> 
              inRange x && wellFormedLabel l &&
              wellFormedStack xs (n-1)
           | 1 % (Ret (lab, (addr, True)) : xs) -> 
              inRange addr && wellFormedLabel lab &&
              wellFormedStack xs (n-1)
           | Ret (_, (_, False)) : _ -> False
    end

sig wellFormed :: AS -> Bool
fun wellFormed as = 
    let' (AS mem instrs stack pc) = as in 
    let' (Atom pcLab addr) = pc in 
#ifdef STARTANY
    inRange addr &&
#else
    addr == 0 && isLow pcLab &&
#endif
    wellFormedMemory mem MEMLENGTH &&
    length instrs PROGLENGTH &&
    wellFormedStack stack STACKLENGTH &&
    runsLong RUNLENGTH (AS mem instrs stack (Atom pcLab addr))

sig indistState :: AS -> AS -> Bool
fun indistState as1 as2 = 
    let' (AS m1 i1 s1 pc1) = as1 in
    let' (AS m2 i2 s2 pc2) = as2 in
    indistAtom pc1 pc2 && indistAtomList m1 m2 && indistInstrList i1 i2 
#ifdef EQUIVFULL
    -- Full
    && if isHighAtom pc1 then indistStkCrop s1 s2 else indistStkLow s1 s2
#else
    -- Low, does the same as EQUIVFULL is STARTANY is *not* defined.
    && indistStkLow s1 s2
#endif

sig statePred :: AS -> AS -> Bool
fun statePred as1 as2 = 
    wellFormed as1 && indistState as1 as2

