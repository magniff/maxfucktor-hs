{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Maxfucktor.Generator (renderProgram) where


import Maxfucktor.AST
import Control.Monad.State (State, get, put, evalState)


-- The list of strings type
type Strings = [[Char]]


data Op = OpAdd | OpSub deriving (Eq, Show)


-- This type is used to enumerate lables in resulting ASM code
newtype BId = BId Int deriving Eq


instance Show BId where
    show (BId value) = "l" ++ show value 


instance Enum BId where
    toEnum = BId
    fromEnum (BId a) = a
    succ (BId value) = BId $ succ value
    pred (BId value) = BId (if value /= 0 then pred value else 0)


-- Converts a single "optimized" AST node into ASM code building action
renderNode :: AST Optimized -> State BId Strings
renderNode node =
    case node of
        Inc rep ->
            return ["add byte [rsi], byte " ++ show rep]
        Dec rep ->
            return ["sub byte [rsi], byte " ++ show rep]
        GoRight rep ->
            return ["add rsi, " ++ show rep]
        GoLeft  rep ->
            return ["sub rsi, " ++ show rep]
        Output  rep ->
            return $
            concat
                [
                    [ "mov rax, 1", "syscall" ] | counter <- [1..rep]
                ]
        Input  rep ->
            return $
            concat [
                [
                    "mov rax, 0",
                    "mov rdi, 0",
                    "syscall",
                    "mov rdi, 1"
                ] | counter <- [1..rep]
            ]
        Mul shift0 shift1 mul0 mul1 ->
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
        Add value ->
            do
                -- call the 'next' function
                current_id <- get
                put $ succ current_id
                -- end of the 'next' invocation
                return $ renderSubAdd value current_id OpAdd
        Sub value ->
            do
                -- call the 'next' function
                current_id <- get
                put $ succ current_id
                -- end of 'next' invocation
                return $ renderSubAdd value current_id OpSub
        Drop ->
            return
            [
                "mov byte [rsi], byte 0"
            ]
        Loop nodes ->
            do
                -- call the 'next' function twice
                thisId <- get
                let contId = succ thisId
                put $ succ contId
                -- end of the 'next' invocations
                innerNodesCode <- mapM renderNode nodes
                return $
                  initLoop thisId contId ++ -- loop initialization code
                  concat innerNodesCode  ++ -- code for the loop body itself
                  loopBack thisId contId    -- loop back code
            where
                initLoop thisId contId = [
                    show thisId ++ ":",
                    "cmp byte [rsi], byte 0",
                    "je " ++ show contId
                    ]
                loopBack thisId contId = [
                    "jmp " ++ show thisId,
                    show contId ++ ":"
                    ]
                jump target_id = ["jmp " ++ show target_id ]
    where
        renderSubAdd :: Int -> BId -> Op -> Strings
        renderSubAdd value contId op =
            let opRepr = if op == OpAdd then "add" else "sub" in
            let header = [";;; Starting Add block"] in
            let footer = [";;; Ending Add block"] in
                header ++
                [
                    "cmp byte [rsi], byte 0",
                    "je .skip" ++ show contId,
                    "movzx r11, byte [rsi]",
                    "mov byte [rsi], byte 0",
                    opRepr ++ " byte [rsi+(" ++ show value ++ ")], r11b",
                    ".skip" ++ show contId
                ] ++
                footer


renderProgram :: [AST Optimized] -> Strings
renderProgram nodes = 
    let code = concat $ evalState (mapM renderNode nodes) (BId 0) in
        code ++ ["jmp exit"]
