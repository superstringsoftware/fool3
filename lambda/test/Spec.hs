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
    let output = TC.as [bold] name
    res <- parseLambda str
    let (success, msg) = assertParseError res expr
    if success then putStrLn $ output ++ ": " ++ TC.as [green] ("OK")
               else putStrLn $ output ++ ": " ++ TC.as [red] ("FAILED\n") ++ "Parsing expression:\n" ++ str ++ "\n" ++ msg
