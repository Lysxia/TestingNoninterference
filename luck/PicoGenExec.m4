dnl To be preprocessed with m4
changequote(`{{', `}}') dnl
define({{STACKLENGTH}}, 10) dnl
define({{MEMLENGTH}}, 10) dnl
define({{PROGLENGTH}}, 10) dnl
define({{RANGE}}, 10) dnl MAX(PROGLENGTH, MEMLENGTH)
define({{RUNLENGTH}}, 10) dnl
define({{MEM}}, {{[| $1 | 0 <= $1 && $1 < MEMLENGTH |]}}) dnl
dnl define({{MEM}}, {{[| $1 | 0 <= $1 && ($1 < MEMLENGTH {1} || {9} $1 < 3) |]}}) dnl
define({{PROG}}, {{[| $1 | 0 <= $1 && $1 < PROGLENGTH |]}}) dnl
dnl
data Label = L | H

data Bool' = T | F

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
      | 1 % L -> True
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

fun wellFormedAtom a =
  let' Atom l n = a in
  inRange n && wellFormedLabel l

sig wellFormedMemory :: [Atom] -> Int -> Bool
fun wellFormedMemory l n =
    if n == 0 then
        case l of
          | [] -> True
          | _  -> False
        end
    else case l of
           | [] -> False
           | a : as -> wellFormedAtom a && wellFormedMemory as (n-1)
    end

sig stackLength :: [StkElt] -> Int -> (Int, Bool')
fun stackLength stack {n @i} =
    case stack of
      | [] -> (n, F)
      | (Ret _ : _ ) -> (n, T)
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

sig finalInstr :: Instr -> Int -> Bool
fun finalInstr i stackSize =
  case i of
  | 5 % Halt -> True
  | 40 % Jump -> stackSize >= 1
  | 40 % Call n True -> [| n | 0 <= n && n < stackSize |]
  | _ -> False
  end

sig wellFormedInstr :: Instr -> Int -> Bool' -> AS -> Bool
fun wellFormedInstr i stackSize returns st =
    case i of
      | 1 % Noop -> True
      | 5 % Halt -> True
      | 40 % Add -> stackSize >= 2
      | 100 % Push (Atom l n) -> inRange n && wellFormedLabel l
      | 40 % Pop ->
ifdef({{BUGPOP}}, {{True}}, {{stackSize >= 1}})
      | 40 % Load -> stackSize >= 1
      | 40 % Store ->
        let' AS m _ s pc = st in
        let' Atom lPC _ = pc in
        case s of
        | Data (Atom lptr ptr) : Data _ : _ ->
          case nth ptr m of
          | Just (Atom l' n') ->
ifdef({{BUGWDOWNHIGHPC}},
            {{}},
            {{labelLeq lPC l' &&}})
ifdef({{BUGWDOWNHIGHPTR}},
            {{True}},
            {{labelLeq lptr l'}})
          | Nothing -> False
          end
        | _ -> False
        end
      | 40 % Jump -> stackSize >= 1
      | 40 % Call n True -> [| n | 0 <= n && n < stackSize |]
      | Call _ False -> False
      | 40 % Return True -> case returns of
        | T -> stackSize >= 1
        | F -> False
        end
      | Return False -> False
    end

sig wellFormedMultiInstr :: Instr -> Instr -> Int -> AS -> Bool
fun wellFormedMultiInstr i j stackSize st =
  case i of
  | 1 % Push (Atom l n) ->
      case j of
      | 1 % Load -> MEM(n) && wellFormedLabel l
      | 3 % Store -> stackSize >= 1 && MEM(n) &&
        let' AS m _ s pc = st in
        let' Atom lPC _ = pc in
        case nth n m of
        | Just (Atom l' n') ->
ifdef({{BUGWDOWNHIGHPC}},
          {{}},
          {{labelLeq lPC l' &&}})
ifdef({{BUGWDOWNHIGHPTR}},
          {{True}},
          {{labelLeq l l'}})
        | Nothing -> False
        end
      | 1 % Jump -> PROG(n) && wellFormedLabel l
      | 1 % Call m True -> PROG(n) && [|m| 0 <= m && m < stackSize|] && wellFormedLabel l
      | _ -> False
      end
  | 0 % _ -> False
  end

sig wellFormedInstrs :: [Bool'] -> [Instr] -> Int -> Int -> Bool' -> AS -> Bool
fun wellFormedInstrs iFlags instrs {addr @i} stackSize returns st =
    case (instrs, iFlags) of
      | (i : is, f : fs) ->
        if addr == 0 then
          case f of
          | T -> True
          | F ->
            case is of
            | [] ->
              wellFormedInstr i stackSize returns st
              {1} || {9}
              finalInstr i stackSize
            | j : _ ->
              wellFormedInstr i stackSize returns st
              {10} || {6}
              wellFormedMultiInstr i j stackSize st
            end
          end
        else
          wellFormedInstrs fs is (addr-1) stackSize returns st
      | _ -> True
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
  case l of
  | h : t ->
    if n == 0 then
      Just (x : t)
    else
      case putNth (n-1) x t of
      | Just t' -> Just (h : t')
      | Nothing -> Nothing
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
ifdef({{BUGARITH}},
    {{Atom L (x1 + x2)}},
    {{Atom (join l1 l2) (x1 + x2)}})

sig getReturn :: [StkElt] -> Maybe ((Label, (Int, Bool)), [StkElt])
fun getReturn s =
  case s of
  | Data _ : s -> getReturn s
  | [] -> Nothing
  | Ret r : s -> Just (r, s)
  end

sig step :: Int -> [Bool'] -> AS -> Bool
fun step len iFlags st =
    let' (AS m is s pc) = st in
    let' (Atom lab addr) = pc in
    case nth addr is of
    | Just i ->
      case i of
      | Noop -> runsLong len iFlags (AS m is s (Atom lab (addr+1)))
      | Add  ->
        case s of
        | (Data a1:Data a2:s') ->
          runsLong len iFlags (AS m is (Data (add a1 a2):s') (Atom lab (addr+1)))
        | _ -> True
        end
      | Push x ->
ifdef({{BUGPUSH}},
        {{let' Atom l n = x in
         runsLong len iFlags (AS m is (Data (Atom L n):s) (Atom lab (addr+1)))}},
        {{runsLong len iFlags (AS m is (Data x:s) (Atom lab (addr+1)))}})
      | Pop  ->
        case s of
        | _ : s' -> runsLong len iFlags (AS m is s' (Atom lab (addr+1)))
        | _ -> True
        end
      | Load ->
        case s of
        | Data a : s' ->
          let' (Atom lptr ptr) = a in
          case nth ptr m of
          | Just (Atom ldata d) ->
              runsLong len iFlags
                (AS
                  m
                  is
ifdef({{BUGLOAD}},
                  {{(Data (Atom ldata d):s')}},
                  {{(Data (Atom (join lptr ldata) d):s')}})
                  (Atom lab (addr+1)))
          | _ -> True
          end
--          nthLoad ptr m (len, m, is, lptr, s', (Atom lab (addr+1)))
        | _ -> True
        end
      | Store ->
        case s of
        | Data (Atom lptr ptr) : Data (Atom l n) : s' ->
ifdef({{BUGSTOREVALUE}},
          {{case putNth ptr (Atom L n) m of}},
  ifdef({{BUGSTOREPOINTER}},
          {{case putNth ptr (Atom (join lab l) n) m of}},
    ifdef({{BUGSTOREPC}},
          {{case putNth ptr (Atom (join lptr l) n) m of}},
          {{case putNth ptr (Atom (join (join lptr lab) l) n) m of}})))
          | Just m' -> runsLong len iFlags (AS m' is s' (Atom lab (addr+1)))
          | Nothing -> True
          end
        | _ -> True
        end
      | Jump ->
        case s of
        | Data (Atom labPtr ptr) : s' ->
          runsLong len iFlags
            (AS
              m
              is
              s'
              (Atom
                ifdef({{BUGJUMPNORAISE}},
                  {{lab}},
                  ifdef({{BUGJUMPLOWER}},
                    {{labPtr}},
                    {{(join lab labPtr)}}
                  )
                )
                ptr
              )
            )
        | _ -> True
        end
      | Call n True ->
        case s of
        | Data (Atom labPtr ptr) : s' ->
          case putNth n (Ret (lab, (addr+1, True))) s' of
          | Just s'' -> runsLong len iFlags (AS m is s''
              (Atom
                ifdef({{BUGCALL}}, {{labPtr}}, {{(join labPtr lab)}})
                ptr))
          | _ -> True
          end
        | _ -> True
        end
      | Return True ->
        case s of
        | Data (Atom lx x) : s' ->
          case getReturn s' of
          | Just ((retl, (retptr, True)), s'') ->
            runsLong len iFlags (AS m is
              (Data (Atom
                      ifdef({{BUGRETURN}},{{lx}},{{(join lx lab)}})
                      x)
               : s'')
              (Atom retl retptr))
          | _ -> True
          end
        | _ : Ret (_, (_, False)) : _ -> False
        | _ -> True
        end
      | Call _ False -> False
      | Return False -> False
      | Halt -> True
      end
    | _ -> True
    end

sig setFlag :: Int -> [Bool'] -> [Bool']
fun setFlag n fs =
  case fs of
  | [] -> []
  | f : fs -> if n == 0 then T : fs else f : setFlag (n-1) fs
  end

sig runsLong :: Int -> [Bool'] -> AS -> Bool
fun runsLong len iFlags st =
    if len <= 0 then True
    else
      let' (AS m is s pc) = st in
      let' (Atom lab addr) = pc in
      let' (stackL, returns) = stackLength s 0 in
      wellFormedInstrs iFlags is addr stackL returns st &&
      step (len-1) (setFlag addr iFlags) (AS m is s (Atom lab addr))

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

sig replicate :: Int -> a -> [a]
fun replicate n x =
  if n == 0
  then []
  else x : replicate (n-1) x

sig wellFormed :: AS -> Bool
fun wellFormed as =
    let' (AS mem instrs stack pc) = as in
    let' (Atom pcLab addr) = pc in
    ifdef(STARTANY, {{inRange addr &&}}, {{addr == 0 && isLow pcLab &&}})
    wellFormedMemory mem MEMLENGTH &&
    length instrs PROGLENGTH &&
    wellFormedStack stack STACKLENGTH &&
    runsLong RUNLENGTH (replicate PROGLENGTH F) (AS mem instrs stack (Atom pcLab addr))

sig indistState :: AS -> AS -> Bool
fun indistState as1 as2 =
    let' (AS m1 i1 s1 pc1) = as1 in
    let' (AS m2 i2 s2 pc2) = as2 in
    indistAtom pc1 pc2 && indistAtomList m1 m2 && indistInstrList i1 i2
    ifdef(EQUIVFULL,
    {{-- Full
     && if isHighAtom pc1 then indistStkCrop s1 s2 else indistStkLow s1 s2}},
    {{-- Low, does the same as EQUIVFULL is STARTANY is *not* defined.
     && indistStkLow s1 s2}})

sig statePred :: AS -> AS -> Bool
fun statePred as1 as2 =
    wellFormed as1 && indistState as1 as2

