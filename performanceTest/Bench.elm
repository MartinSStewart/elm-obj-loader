module Bench exposing (..)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark)
import TestfileAsString exposing (objFile)
import CombineParser
import OBJ.Parser exposing (parse)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "Parser vs Combine"
        [ Benchmark.compare "Parse obj file"
            (benchmark "combine" (\() -> CombineParser.parse objFile))
            (benchmark "parser" (\() -> parse objFile))
        ]
