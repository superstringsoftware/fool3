import Util.PrettyPrinting as TC
import SurfaceLanguage.Lambda.ParserNew
import Data.Text
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import State
import CoreLambda.Syntax
import Text.Pretty.Simple (pShow)

import qualified Data.Text.Lazy as TL

main :: IO ()
main = do
    putStrLn "\nRunning Lambda Parsing Test Cases\n\n"
    runAllLPC

 
-------------------- TESTING LAMBDA PARSING --------------------
lambdaParsingCases = [
        ("Simple untyped pattern match 1", "id x = x", PatternMatch [ App ( VarId "id" ) [ VarId "x" ] ] ( VarId "x" )),
        ("Simple untyped pattern match 2", "fact 0 = 1", PatternMatch [ App ( VarId "fact" ) [ Lit ( LInt 0 )  ] ] ( Lit ( LInt 1 )  )),
        ("Simple untyped pattern match with recursion", "fact n = n * fact (n-1)", 
            PatternMatch [ App ( VarId "fact" ) [ VarId "n" ] ] 
                ( BinaryOp "*" ( VarId "n" ) 
                    ( App ( VarId "fact" ) 
                    [ BinaryOp "-" ( VarId "n" ) 
                        ( Lit ( LInt 1 ) )
                    ]
                    )
                ) 
        ),
        ("Simple untyped function as lambda", "id = \\x . x",
            Let 
                ( Field 
                    { fieldName = "id" 
                    , fieldType = ToDerive
                    , fieldValue = Lam 
                        { boundVars = 
                            [ Field 
                                { fieldName = "x" 
                                , fieldType = ToDerive
                                , fieldValue = EMPTY
                                } 
                            ]
                        , lambdaBody = VarId "x" 
                        , lambdaType = ToDerive
                        , lamPredicates = []
                        } 
                    } 
                )
        ),
        -- TYPES -------------------------------------------------------------------
        ("Simple SumType (Bool)", "Bool : Type = { True, False }", 
            Let 
            ( Field 
                { fieldName = "Bool" 
                , fieldType = SmallType 
                , fieldValue = Tuple "" 
                    [ Cons [] "True" ToDerive
                    , Cons [] "False" ToDerive 
                    ] ToDerive
                } 
            )
        ),
        ("Parametric SumType (Maybe a)", "Maybe : Type = \\a . { Nothing, Just :a }", 
            Let 
            ( Field 
                { fieldName = "Bool" 
                , fieldType = SmallType 
                , fieldValue = Tuple "" 
                    [ Cons [] "True" ToDerive
                    , Cons [] "False" ToDerive 
                    ] ToDerive
                } 
            )
        ),
        ("Parametric SumType (Maybe a) defined as a pattern match", "Maybe a = { Nothing, Just :a }", 
            Let 
            ( Field 
                { fieldName = "Bool" 
                , fieldType = SmallType 
                , fieldValue = Tuple "" 
                    [ Cons [] "True" ToDerive
                    , Cons [] "False" ToDerive 
                    ] ToDerive
                } 
            )
        ),
        ("Type signature 0", "Bool : Type", VarDefinition ( Var "Bool" SmallType )),
        ("Type signature 1", "Maybe : Type -> Type", VarDefinition ( Var "Maybe" ( TArr SmallType SmallType ))),
        ("Type signature 2", "plus : Int -> Float -> Maybe a", 
          VarDefinition 
            ( Var "plus" 
                ( TArr ( TCon "Int" ) 
                    ( TArr ( TCon "Float" ) 
                        ( TApp ( TCon "Maybe" ) 
                            [ TVar ( Var "a" ToDerive ) ]
                        )
                    )
                )
            )
        ),
        ("Type signature 3", "length : List a -> Int", VarDefinition 
            ( Var "length" 
                ( TArr 
                    ( TApp ( TCon "List" ) 
                        [ TVar ( Var "a" ToDerive ) ]
                    ) ( TCon "Int" )
                )
            )
        ),
        ("Type signature 4", "Pair : Type -> Type -> Type", VarDefinition 
            ( Var "Pair" 
                ( TArr SmallType ( TArr SmallType SmallType ) )
            )
        ),
        ("Type signature 5", "Weird : Int -> Pair a b -> Type", VarDefinition 
            ( Var "Weird" 
                ( TArr ( TCon "Int" ) 
                    ( TArr 
                        ( TApp ( TCon "Pair" ) 
                            [ TVar ( Var "a" ToDerive )
                            , TVar ( Var "b" ToDerive )
                            ] 
                        ) SmallType
                    )
                )
            )
        ),
        ("Type signature 6", "map : (a->b) -> List a -> List b", EMPTY),
        ("New function definition format", "map:(List b) { func:a->b, ls:List a } ", EMPTY)

    ]

runAllLPC = mapM_ runPTC lambdaParsingCases

-- parsing a single string with our parser
parseLambda str = liftIO $ runIntState (parseToplevel (pack str)) emptyIntState

-- error messages handling, returns (True, message) if correct comparison between the parse result and the expected type or (False, message) otherwise
assertParseError (Left err) _ = (False, TC.as [red] ("Parse error: ") ++ (show err))
assertParseError (Right e) e1 = if (e == e1) then (True, TC.as [green] ("OK")) 
    else (False, "parsed as\n" ++ (TL.unpack $ pShow e) ++ "\nbut expected\n" ++ (TL.unpack $ pShow e1)) 

-- run one test case
runPTC (name, str, expr) = do
    let output = TC.as [bold] name
    res <- parseLambda str
    let (success, msg) = assertParseError res expr
    if success then putStrLn $ output ++ ": " ++ TC.as [green] ("OK")
               else putStrLn $ output ++ ": " ++ TC.as [red] ("FAILED\n") ++ "Parsing expression:\n" ++ str ++ "\n" ++ msg
