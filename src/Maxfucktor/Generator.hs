{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Maxfucktor.Generator (renderProgram) where


import qualified Maxfucktor.Optimizer as Opt
import Control.Monad.State (State, get, put, evalState)


type Strings = [[Char]]


data Op = Add | Sub deriving (Eq, Show)
newtype BlockId = BlockId Int deriving Eq


nextContid :: BlockId -> BlockId
nextContid (BlockId value) = BlockId $ value + 1


showContid :: BlockId -> String
showContid (BlockId value) = "l" ++ show value


renderNode :: Opt.AST -> State BlockId Strings
renderNode node =
    case node of
        Opt.Inc rep ->
            return ["add byte [rsi], byte " ++ show rep]
        Opt.Dec rep ->
            return ["sub byte [rsi], byte " ++ show rep]
        Opt.GoRight rep ->
            return ["add rsi, " ++ show rep]
        Opt.GoLeft  rep ->
            return ["sub rsi, " ++ show rep]
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
                toASMBlock mul shift = 
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
        Opt.Drop ->
            return
            [
                "mov byte [rsi], byte 0"
            ]
        Opt.Loop nodes ->
            do
                this_id <- get
                put $ nextContid this_id
                cont_id <- get
                put $ nextContid cont_id
                innerNodesCode <- mapM renderNode nodes
                return $ initLoop this_id cont_id ++ concat innerNodesCode ++ loopBack this_id cont_id
            where
                initLoop this_id cont_id = [
                    showContid this_id ++ ":",
                    "cmp byte [rsi], byte 0",
                    "je " ++ showContid cont_id
                    ]
                loopBack this_id cont_id = [
                    "jmp " ++ showContid this_id,
                    showContid cont_id ++ ":"
                    ]
                jump target_id = ["jmp " ++ showContid target_id ]
    where
        renderSubAdd :: Int -> BlockId -> Op -> Strings
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


renderProgram :: [Opt.AST] -> Strings
renderProgram nodes = 
    let code = concat $ evalState (mapM renderNode nodes) (BlockId 1) in
        code ++ ["jmp exit"]
