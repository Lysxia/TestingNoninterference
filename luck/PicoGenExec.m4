dnl To be preprocessed with m4
changequote(`{{', `}}')dnl
define({{STACK_LENGTH}}, 10)dnl
define({{MEM_LENGTH}}, 10)dnl
define({{PROG_LENGTH}}, 10)dnl
define({{RANGE}}, 10)dnl MAX(PROG_LENGTH, MEM_LENGTH)
define({{RUN_LENGTH}}, 10)dnl
dnl define({{MEM}}, {{(0 <= $1 && ($1 < MEM_LENGTH {1} || {9} $1 < 3)) !$1}}) dnl
dnl
data Label = L | H

data Atom = Atom Label Int

data Instr
    = Noop
    | Add
    | Push Atom
    | Pop
    | Load
    | Store
    | Jump
    | Call Int Bool
    | Return Bool
    | Halt

data StkElt = Data Atom
            | Ret  (Label, (Int, Bool))

data AS = AS [Atom] [Instr] [StkElt] Atom

sig replicate :: Int -> a -> [a]
fun replicate n x =
  if n == 0
  then []
  else x : replicate (n-1) x

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
    | H ->
        case m of
        | H -> True
        | L -> False
        end
    | L -> True
    end

sig isHighAtom :: Atom -> Bool
fun isHighAtom pc =
    let' (Atom l _) = pc in
    isHigh l

-- Does *not* instantiate
sig inRange :: Int -> Bool
fun inRange x = 0 <= x && x < RANGE

sig isProgAddr :: Int -> Bool
fun isProgAddr x = 0 <= x && x < PROG_LENGTH

sig isMemAddr :: Int -> Bool
fun isMemAddr x = 0 <= x && x < MEM_LENGTH

sig nth :: Int -> [a] -> Maybe a
fun nth n l =
    if n == 0 then
        case l of
        | i:_ -> Just i
        | _ -> Nothing
        end
    else
        case l of
        | _:is -> nth (n-1) is
        | _ -> Nothing
        end

sig indistAtom :: Atom -> Atom -> Bool
fun indistAtom a1 a2 =
    let' (Atom l1 v1) = a1 in
    let' (Atom l2 v2) = a2 in
    eqL l1 l2 &&
    if isLow l1 then
        fix {2 :: (v1 == v2) !v1} !v2
    else
        inRange v1 !v1 && inRange v2 !v2

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
fun length l n =
    if n == 0 then
        case l of
        | [] -> True
        | _  -> False
        end
    else
        case l of
        | [] -> False
        | (x : xs) -> length xs (n-1)
        end

fun wellFormedAtom a =
    let' Atom l n = a in
    inRange n &&
    wellFormedLabel l

sig wellFormedMemory :: [Atom] -> Bool
fun wellFormedMemory l =
    case l of
    | 1 % [] -> True
    | MEM_LENGTH % a : as -> wellFormedAtom a && wellFormedMemory as
    end

sig wellFormedStack :: [StkElt] -> Bool
fun wellFormedStack st =
    case st of
    | 1 % [] -> True
    | STACK_LENGTH % (x : xs) ->
        case x of
        | 5 % Data (Atom l x) ->
            inRange x &&
            wellFormedLabel l
        | 1 % Ret (l, (addr, True)) ->
            isProgAddr addr &&
            wellFormedLabel l
        | Ret (_, (_, False)) -> False
        end &&
        wellFormedStack xs
    end

sig stackLength :: [StkElt] -> Int -> (Int, Bool)
fun stackLength stack n =
    case stack of
    | [] -> (n, False)
    | (Ret _ : _ ) -> (n, True)
    | (Data _ : s) -> stackLength s (n+1)
    end

sig indistInstrList :: [Instr] -> [Instr] -> Bool
fun indistInstrList i1 i2 =
    case (i1, i2) of
    | ([], []) -> True
    | (h1:t1, h2:t2) -> indistInstr h1 h2 && indistInstrList t1 t2
    | _ -> False
    end

sig finalInstr :: Instr -> Int -> Bool
fun finalInstr i stackSize =
    case i of
    | 5 % Halt -> True
    | 40 % Jump -> stackSize >= 1
    | 40 % Call n True -> (0 <= n && n < stackSize) !n
    | _ -> False
    end

sig wellFormedInstr :: Instr -> Int -> Bool -> AS -> Bool
fun wellFormedInstr i stackSize returns st =
    case i of
    | 1 % Noop -> True
    | 5 % Halt -> True
    | 40 % Add -> stackSize >= 2
    | 100 % Push (Atom l n) -> inRange n !n && wellFormedLabel l
    | 40 % Pop ->
ifdef({{BUGPOP}}, {{True}}, {{stackSize >= 1}})
    | 40 % Load -> stackSize >= 1
    | 40 % Store ->
        let' AS m _ s pc = st in
        let' Atom lPC _ = pc in
        case s of
        | Data (Atom lptr ptr) : Data _ : _ ->
            isMemAddr ptr !ptr &&
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
    | 40 % Call n True -> (0 <= n && n < stackSize) !n
    | Call _ False -> False
    | 40 % Return True ->
        case returns of
        | True -> stackSize >= 1
        | False -> False
        end
    | Return False -> False
    end

sig wellFormedMultiInstr :: Instr -> Instr -> Int -> AS -> Bool
fun wellFormedMultiInstr i j stackSize st =
    case i of
    | 100 % Push (Atom l n) ->
        case j of
        | 1 % Load -> isMemAddr n !n && wellFormedLabel l
        | 3 % Store -> stackSize >= 1 && isMemAddr n !n &&
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
        | 1 % Jump -> isProgAddr n !n && wellFormedLabel l
        | 1 % Call m True ->
            isProgAddr n !n &&
            (0 <= m && m < stackSize) !m &&
            wellFormedLabel l
        | _ -> False
        end
    | 1 % _ -> False
    end

sig wellFormedInstrs :: [Bool] -> [Instr] -> Int -> Int -> Bool -> AS -> Bool
fun wellFormedInstrs iFlags instrs addr stackSize returns st =
    case (instrs, iFlags) of
    | (i : is, f : fs) ->
        if addr == 0 then
            case f of
            | True -> True
            | False ->
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

sig insertNth :: Int -> a -> [a] -> Maybe [a]
fun insertNth n x l =
    if n == 0 then
        Just (x : l)
    else
        case l of
        | h : t ->
            case insertNth (n-1) x t of
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
    let' () = () !x1 !x2 in
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

sig step :: Int -> [Bool] -> AS -> Bool
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
                (isMemAddr ptr {99} || {1} True) !ptr &&
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
            | _ -> True
            end
        | Store ->
            case s of
            | Data (Atom lptr ptr) : Data (Atom l n) : s' ->
                (isMemAddr ptr {99} || {1} True) !ptr &&
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
                (isProgAddr ptr {99} || {1} True) !ptr &&
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
                let' () = () !n in
                (isProgAddr ptr {99} || {1} True) !ptr &&
                case insertNth n (Ret (lab, (addr+1, True))) s' of
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
                    (isProgAddr retptr {99} || {1} True) !retptr &&
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

sig setFlag :: Int -> [Bool] -> [Bool]
fun setFlag n fs =
    case fs of
    | [] -> []
    | f : fs -> if n == 0 then True : fs else f : setFlag (n-1) fs
    end

sig runsLong :: Int -> [Bool] -> AS -> Bool
fun runsLong len iFlags st =
    if len <= 0 then True
    else
        let' (AS m is s pc) = st in
        let' (Atom lab addr) = pc in
        let' (stackL, returns) = stackLength s 0 in
        wellFormedInstrs iFlags is addr stackL returns st &&
        step (len-1) (setFlag addr iFlags) st

sig wellFormed :: AS -> Bool
fun wellFormed as =
    let' (AS mem instrs stack pc) = as in
    let' (Atom pcLab addr) = pc in
    length instrs PROG_LENGTH &&
    wellFormedMemory mem &&
    wellFormedStack stack &&
    (ifdef(STARTANY, {{isProgAddr addr}}, {{addr == 0 && isLow pcLab}})) !addr &&
    runsLong RUN_LENGTH (replicate PROG_LENGTH False) as

sig indistState :: AS -> AS -> Bool
fun indistState as1 as2 =
    let' (AS m1 i1 s1 pc1) = as1 in
    let' (AS m2 i2 s2 pc2) = as2 in
    indistAtom pc1 pc2 && indistAtomList m1 m2 && indistInstrList i1 i2
    ifdef(EQUIVFULL,
    {{-- Full
    && if isHighAtom pc1 then indistStkCrop s1 s2 else indistStkLow s1 s2}},
    {{-- Low, does the same as EQUIVFULL if STARTANY is *not* defined.
    && indistStkLow s1 s2}})

sig statePred :: AS -> AS -> Bool
fun statePred {as1 :: 100} {as2 :: 100} =
    wellFormed as1 && indistState as1 as2
