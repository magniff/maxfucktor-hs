--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Maxfucktor.Generator where


import qualified Maxfucktor.Optimizer as Opt
import Control.Monad.State (State, get, put, evalState)


type Strings = [[Char]]


data Op = Add | Sub deriving Eq
data ContID = Local Int | Global


initId :: ContID
initId = Local 0


nextContid :: ContID -> ContID
nextContid cont_id =
    case cont_id of
        Local value -> Local $ value + 1
        Global -> error "Oooops, should not be there!"


showContid :: ContID -> String
showContid cont_id = 
    case cont_id of
        Global -> "exit"
        Local value -> "l" ++ show value


renderNode :: Opt.AST -> State ContID Strings
renderNode node =
    case node of
        Opt.Inc rep ->
            return ["add byte [rsi], byte " ++ show rep]
        Opt.Dec rep ->
            return ["sub byte [rsi], byte " ++ show rep]
        Opt.GoRight rep ->
            return ["add rsi, " ++ show rep]
        Opt.Output  rep ->
            return $
            concat
                [
                    [ "mov rax, 1", "syscall" ] | counter <- [1..rep]
                ]
        Opt.Input  rep ->
            return $
            concat [
                [
                    "mov rax, 0",
                    "mov rdi, 0",
                    "syscall",
                    "mov rdi, 1"
                ] | counter <- [1..rep]
            ]
        Opt.Mul shift0 shift1 mul0 mul1 ->
            return $
            let header = [";;;Start Mul block" ] in
            let footer = [";;;End Mul block" ] in
                header ++
                toASMBlock mul0 shift0 ++
                toASMBlock mul1 shift1 ++
                ["mov [rsi], byte 0"] ++
                footer
            where
                toASMBlock shift mul = 
                    ["movzx rax, byte [rsi]"] ++
                    (
                        if mul > 1
                        then [
                            "movzx r12, byte " ++ show mul,
                            "mul r12b"
                        ]
                        else mempty 
                    ) ++
                    [
                        "add byte [rsi+" ++ show shift ++"], al"
                    ]
        Opt.Add value ->
            do
                current_id <- get
                put $ nextContid current_id
                return $ renderSubAdd value current_id Add
        Opt.Sub value ->
            do
                current_id <- get
                put $ nextContid current_id
                return $ renderSubAdd value current_id Sub
        Opt.Loop nodes ->
            do
                this_id <- get
                put $ nextContid this_id
                cont_id <- get
                undefined 
            where
                initLoop this_id cont_id = [
                    showContid this_id ++ ":",
                    "cmp byte [rsi], byte 0",
                    "je " ++ showContid cont_id
                    ]
                loopBack this_id cont_id = [
                    "jmp " ++ showContid this_id,
                    show cont_id ++ ":"
                    ]
                jump target_id = ["jmp " ++ showContid target_id ]
-- def visit_Loop(node: Loop, cont_id: Optional[str] = None) -> VisitorOutput:
--     this_id = next(ids)
--     cont_id = cont_id or next(ids)

--     if cont_id != GLOBAL_EXIT:
--         yield CommandInitLoop(this_id=this_id, cont_id=cont_id)  # type: ignore
--     for subnode in node.contains:
--         yield from visit(subnode, cont_id=None)
--     if cont_id != GLOBAL_EXIT:
--         yield CommandLoopBack(this_id=this_id, cont_id=cont_id)  # type: ignore
--     else:
--         yield CommandJump(target_id=GLOBAL_EXIT)

    where
        renderSubAdd :: Int -> ContID -> Op -> Strings
        renderSubAdd shift cont_id op =
            let opRepr = if op == Add then "add" else "sub" in
            let header = [";;; start Add block"] in
            let footer = [";;; end Add block"] in
                header ++
                [
                    "cmp byte [rsi], byte 0",
                    "je .skip" ++ showContid cont_id,
                    "movzx r11, byte [rsi]",
                    "mov byte [rsi], byte 0",
                    opRepr ++ " byte [rsi+(" ++ show shift ++ ")], r11b",
                    ".skip" ++ showContid cont_id
                ] ++
                footer

renderProgram :: Opt.AST -> Strings
renderProgram node = evalState (renderNode node) Global
