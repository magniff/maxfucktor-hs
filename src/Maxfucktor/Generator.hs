{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Maxfucktor.Generator (renderProgram) where


import qualified Maxfucktor.Optimizer as Opt
import Control.Monad.State (State, get, put, evalState)


type Strings = [[Char]]


data Op = Add | Sub deriving (Eq, Show)


-- This type is used to enumerate lables in resulting ASM code
newtype BId = BId Int deriving Eq


-- It might be a good idea to implement the "Enum BId" instead of this
nextContid :: BId -> BId
nextContid (BId value) = BId $ value + 1


instance Show BId where
    show (BId value) = "l" ++ show value 


-- Converts a single "optimized" AST node into ASM code building action
renderNode :: Opt.AST -> State BId Strings
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
            let header = [";;; Starting Mul block" ] in
            let footer = [";;; Ending Mul block" ] in
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
                -- call the 'next' function
                current_id <- get
                put $ nextContid current_id
                -- end of 'next' invocation
                return $ renderSubAdd value current_id Add
        Opt.Sub value ->
            do
                -- call the 'next' function
                current_id <- get
                put $ nextContid current_id
                -- end of 'next' invocation
                return $ renderSubAdd value current_id Sub
        Opt.Drop ->
            return
            [
                "mov byte [rsi], byte 0"
            ]
        Opt.Loop nodes ->
            do
                -- call the 'next' function twice
                this_id <- get
                put $ nextContid this_id
                cont_id <- get
                put $ nextContid cont_id
                -- end of 'next' invocations
                innerNodesCode <- mapM renderNode nodes
                return $
                  initLoop this_id cont_id ++ -- loop initialization code
                  concat innerNodesCode    ++ -- code for the loop body itself
                  loopBack this_id cont_id    -- loop back code
            where
                initLoop this_id cont_id = [
                    show this_id ++ ":",
                    "cmp byte [rsi], byte 0",
                    "je " ++ show cont_id
                    ]
                loopBack this_id cont_id = [
                    "jmp " ++ show this_id,
                    show cont_id ++ ":"
                    ]
                jump target_id = ["jmp " ++ show target_id ]
    where
        renderSubAdd :: Int -> BId -> Op -> Strings
        renderSubAdd value cont_id op =
            let opRepr = if op == Add then "add" else "sub" in
            let header = [";;; Starting Add block"] in
            let footer = [";;; Ending Add block"] in
                header ++
                [
                    "cmp byte [rsi], byte 0",
                    "je .skip" ++ show cont_id,
                    "movzx r11, byte [rsi]",
                    "mov byte [rsi], byte 0",
                    opRepr ++ " byte [rsi+(" ++ show value ++ ")], r11b",
                    ".skip" ++ show cont_id
                ] ++
                footer


renderProgram :: [Opt.AST] -> Strings
renderProgram nodes = 
    let code = concat $ evalState (mapM renderNode nodes) (BId 1) in
        code ++ ["jmp exit"]
