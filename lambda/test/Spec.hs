import Util.PrettyPrinting as TC
import SurfaceLanguage.Lambda.Parser
import Data.Text
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import State
import Core.Syntax
import Text.Pretty.Simple (pShow)

import qualified Data.Text.Lazy as TL

main :: IO ()
main = do
    putStrLn "\n\nRunning Lambda Parsing Test Cases\n"
    runAllLPC

-------------------- NEW SYNTAXIS: TESTING LAMBDA PARSING --------------------
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
        ("Untyped pattern match with 2 variables", "map _ [] = []", PatternMatch 
            [ App ( VarId "map" ) 
                [ VarId "_" 
                , Lit ( LList [] )
                ] 
            ] 
            ( Lit ( LList [] ) )),
        -- ("Untyped pattern match with 2 variables", "map f x1 = (f x):::(map f xs)", EMPTY),
        ("Untyped pattern match with 2 variables", "map f (x::xs) = (f x)::(map f xs)", PatternMatch 
            [ App ( VarId "map" ) 
                [ VarId "f" 
                , BinaryOp "::" ( VarId "x" ) ( VarId "xs" )
                ] 
            ] 
            ( BinaryOp "::" 
                ( App ( VarId "f" ) [ VarId "x" ] ) 
                ( App ( VarId "map" ) 
                    [ VarId "f" 
                    , VarId "xs" 
                    ] 
                )
            )
        ),
        ("", "Bool:Type = {True, False}", Binding ( Var "Bool" SmallType ) 
            ( Lambda 
                { params = []
                , body = Rec 
                    [ Field 
                        { fieldName = "True" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    , Field 
                        { fieldName = "False" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ] 
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        -- ("", "Maybe:Type { a } = { Nothing, Just :a }", EMPTY),
        ("", "Maybe:Type { a:Type } = { Nothing, Just { :a } }", Binding ( Var "Maybe" SmallType ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = SmallType
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "Nothing" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    , Field 
                        { fieldName = "Just" 
                        , fieldType = ToDerive
                        , fieldValue = Lam 
                            ( Lambda 
                                { params = 
                                    [ Field 
                                        { fieldName = "" 
                                        , fieldType = TVar ( Var "a" ToDerive )
                                        , fieldValue = EMPTY
                                        } 
                                    ]
                                , body = EMPTY
                                , sig = ToDerive
                                , preds = []
                                } 
                            )
                        } 
                    ] 
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        ("", "Maybe:Type { a } = { Nothing {}, Just { :a } }", Binding ( Var "Maybe" SmallType ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "Nothing" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    , Field 
                        { fieldName = "Just" 
                        , fieldType = ToDerive
                        , fieldValue = Lam 
                            ( Lambda 
                                { params = 
                                    [ Field 
                                        { fieldName = "" 
                                        , fieldType = TVar ( Var "a" ToDerive )
                                        , fieldValue = EMPTY
                                        } 
                                    ]
                                , body = EMPTY
                                , sig = ToDerive
                                , preds = []
                                } 
                            )
                        } 
                    ] 
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        -- ("", "Maybe:Type { a } = { Nothing:Maybe a, Just:(Maybe a) x:a }", EMPTY),
        ("", "plus2:Int { x:Int } = x + 2", Binding 
            ( Var "plus2" ( TCon "Int" ) ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "x" 
                        , fieldType = TCon "Int" 
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = BinaryOp "+" ( VarId "x" ) 
                    ( Lit ( LInt 2 ) )
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        ("", "List:Type { a } = { Nil, Just {:a, :(List a) } }", Binding ( Var "List" SmallType ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "Nil" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    , Field 
                        { fieldName = "Just" 
                        , fieldType = ToDerive
                        , fieldValue = Lam 
                            ( Lambda 
                                { params = 
                                    [ Field 
                                        { fieldName = "" 
                                        , fieldType = TVar ( Var "a" ToDerive )
                                        , fieldValue = EMPTY
                                        } 
                                    , Field 
                                        { fieldName = "" 
                                        , fieldType = TApp ( TCon "List" ) 
                                            [ TVar ( Var "a" ToDerive ) ]
                                        , fieldValue = EMPTY
                                        } 
                                    ] 
                                , body = EMPTY
                                , sig = ToDerive
                                , preds = []
                                } 
                            )
                        } 
                    ] 
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        ("", "func : Int -> List a -> Type", VarDefinition 
            ( Var "func" 
                ( TArr ( TCon "Int" ) 
                    ( TArr 
                        ( TApp ( TCon "List" ) 
                            [ TVar ( Var "a" ToDerive ) ]
                        ) SmallType
                    )
                )
            )
        ),
        ("", "Semigroup : Class { a } = { (+):a { :a, :a } }", Binding 
            ( Var "Semigroup" ( TCon "Class" ) ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "+" 
                        , fieldType = TVar ( Var "a" ToDerive )
                        , fieldValue = Lam 
                            ( Lambda 
                                { params = 
                                    [ Field 
                                        { fieldName = "" 
                                        , fieldType = TVar ( Var "a" ToDerive )
                                        , fieldValue = EMPTY
                                        } 
                                    , Field 
                                        { fieldName = "" 
                                        , fieldType = TVar ( Var "a" ToDerive )
                                        , fieldValue = EMPTY
                                        } 
                                    ] 
                                , body = EMPTY
                                , sig = ToDerive
                                , preds = []
                                } 
                            )
                        } 
                    ]
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        ("", "Semigroup : Class { a } = { (+):a->a->a }", Binding 
            ( Var "Semigroup" ( TCon "Class" ) ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "+" 
                        , fieldType = TArr 
                            ( TVar ( Var "a" ToDerive ) ) 
                            ( TArr 
                                ( TVar ( Var "a" ToDerive ) ) 
                                ( TVar ( Var "a" ToDerive ) )
                            )
                        , fieldValue = EMPTY
                        } 
                    ]
                , sig = ToDerive
                , preds = []
                } 
            )
        ),
        ("", "∃ Semigroup a => Monoid : Class { a } = { E0:a } ", Binding 
            ( Var "Monoid" ( TCon "Class" ) ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "E0" 
                        , fieldType = TVar ( Var "a" ToDerive )
                        , fieldValue = EMPTY
                        } 
                    ]
                , sig = ToDerive
                , preds = 
                    [ Exists 
                        ( TApp ( TCon "Semigroup" ) 
                            [ TVar ( Var "a" ToDerive ) ]
                        )
                    ]
                } 
            )
        ),
        ("", "∃ Semigroup a => Monoid {a} = { E0:a } ", Binding ( Var "Monoid" ToDerive ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = ToDerive
                        , fieldValue = EMPTY
                        } 
                    ]
                , body = Rec 
                    [ Field 
                        { fieldName = "E0" 
                        , fieldType = TVar ( Var "a" ToDerive )
                        , fieldValue = EMPTY
                        } 
                    ]
                , sig = ToDerive
                , preds = 
                    [ Exists 
                        ( TApp ( TCon "Semigroup" ) 
                            [ TVar ( Var "a" ToDerive ) ]
                        )
                    ]
                } 
            )
        ),
        ("Dependent type", "VectorN:Type {a:Type, n:Int} = { VectorN { data:(Array a n), size = n } } ", Binding ( Var "VectorN" SmallType ) 
            ( Lambda 
                { params = 
                    [ Field 
                        { fieldName = "a" 
                        , fieldType = SmallType
                        , fieldValue = EMPTY
                        } 
                    , Field 
                        { fieldName = "n" 
                        , fieldType = TCon "Int" 
                        , fieldValue = EMPTY
                        } 
                    ] 
                , body = Rec 
                    [ Field 
                        { fieldName = "VectorN" 
                        , fieldType = ToDerive
                        , fieldValue = Lam 
                            ( Lambda 
                                { params = 
                                    [ Field 
                                        { fieldName = "data" 
                                        , fieldType = TApp ( TCon "Array" ) 
                                            [ TVar ( Var "a" ToDerive )
                                            , TVar ( Var "n" ToDerive )
                                            ] 
                                        , fieldValue = EMPTY
                                        } 
                                    , Field 
                                        { fieldName = "size" 
                                        , fieldType = ToDerive
                                        , fieldValue = Lam 
                                            ( Lambda 
                                                { params = []
                                                , body = VarId "n" 
                                                , sig = ToDerive
                                                , preds = []
                                                } 
                                            )
                                        } 
                                    ] 
                                , body = EMPTY
                                , sig = ToDerive
                                , preds = []
                                } 
                            )
                        } 
                    ]
                , sig = ToDerive
                , preds = []
                } 
            )
        )
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
    res <- parseLambda str
    let (success, msg) = assertParseError res expr
    if success then putStrLn $ TC.as [green] ("OK") ++ ": " ++ TC.as [dim] str
               else putStrLn $ TC.as [red] ("FAILED: ") ++ str ++ "\n" ++ msg

runPTClong (name, str, expr) = do
    let output = TC.as [bold] name
    res <- parseLambda str
    let (success, msg) = assertParseError res expr
    if success then putStrLn $ output ++ ": " ++ TC.as [green] ("OK") ++ TC.as [dim] (" (" ++ str ++ ")")
               else putStrLn $ output ++ ": " ++ TC.as [red] ("FAILED\n") ++ "Parsing expression:\n" ++ str ++ "\n" ++ msg
