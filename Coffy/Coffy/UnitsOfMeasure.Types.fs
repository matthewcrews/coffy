namespace Coffy.UnitsOfMeasure.Types

open Coffy

type Decision<[<Measure>] 'Measure> =
    | Value of Types.Decision
with

    member this.Name =
        let (Value d) = this
        d.Name

    static member (+) (Value lD:Decision<'Measure>, Value rD:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (lD + rD)

    static member (+) (Value d:Decision<'Measure>, i:int<'Measure>) =
        LinearExpression<'Measure>.Value (d + int i)

    static member (+) (i:int<'Measure>, d:Decision<'Measure>) =
        d + i

    static member (*) (Value d:Decision<'LMeasure>, i:int<'RMeasure>) =
        LinearExpression<'LMeasure 'RMeasure>.Value (d * int i)

    static member (*) (i:int<'LMeasure>, d:Decision<'RMeasure>) =
        d * i

    static member (-) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (l - r)

    static member (-) (Value d:Decision<'Measure>, i:int<'Measure>) =
        LinearExpression<'Measure>.Value (d - int i)

    static member (-) (i:int<'Measure>, Value d:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (int i - d)

    static member (<==) (Value l:Decision<'Measure>, r:int<'Measure>) =
        l <== int r

    static member (<==) (l:int<'Measure>, Value r:Decision<'Measure>) =
        int l <== r

    static member (<==) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        l <== r

    static member (==) (Value l:Decision<'Measure>, r:int<'Measure>) =
        l == int r

    static member (==) (l:int<'Measure>, Value r:Decision<'Measure>) =
        int l == r

    static member (==) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        l == r

    static member (>==) (Value l:Decision<'Measure>, r:int<'Measure>) =
        l >== int r

    static member (>==) (l:int<'Measure>, Value r:Decision<'Measure>) =
        int l >== r

    static member (>==) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        l >== r


and [<NoComparison>]
LinearExpression<[<Measure>] 'Measure> =
    | Value of Types.LinearExpression
    with

        static member inline Zero =
            let expr = Types.LinearExpression (Set.empty, Map.empty, Map.empty, 0)
            LinearExpression<'Measure>.Value expr

        static member (+) (Value lExpr:LinearExpression<'Measure>, Value rExpr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (lExpr + rExpr)

        static member (+) (Value expr:LinearExpression<'Measure>, i:int<'Measure>) =
            LinearExpression<'Measure>.Value (expr + int i)

        static member (+) (i:int<'Measure>, expr:LinearExpression<'Measure>) =
            expr + i

        static member (+) (Value expr:LinearExpression<'Measure>, Decision.Value d:Decision<'Measure>) =
            LinearExpression<'Measure>.Value (expr + d)

        static member (+) (d:Decision<'Measure>, expr:LinearExpression<'Measure>) =
            expr + d

        static member (*) (Value expr:LinearExpression<'LMeasure>, i:int<'RMeasure>) =
            LinearExpression<'Measure>.Value (expr * int i)

        static member (*) (i:int<'LMeasure>, expr:LinearExpression<'RMeasure>) =
            expr * i

        static member (-) (Value expr:LinearExpression<'Measure>, i:int<'Measure>) =
            LinearExpression<'Measure>.Value (expr - int i)

        static member (-) (i:int<'Measure>, Value expr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (int i - expr)

        static member (-) (Value expr:LinearExpression<'Measure>, Decision.Value d:Decision<'Measure>) =
            LinearExpression<'Measure>.Value (expr - d)

        static member (-) (Decision.Value d:Decision<'Measure>, Value expr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (d - expr)

        static member (-) (Value lExpr:LinearExpression<'Measure>, Value rExpr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (lExpr - rExpr)

        static member (<==) (Value l:LinearExpression<'Measure>, r:int<'Measure>) =
            l <== int r

        static member (<==) (l:int<'Measure>, Value r:LinearExpression<'Measure>) =
            int l <== r

        static member (<==) (Value l:LinearExpression<'Measure>, Decision.Value r:Decision<'Measure>) =
            l <== r

        static member (<==) (Decision.Value l:Decision<'Measure>, Value r:LinearExpression<'Measure>) =
            l <== r

        static member (<==) (Value l:LinearExpression<'Measure>, Value r:LinearExpression<'Measure>) =
            l <== r

        static member (==) (Value l:LinearExpression<'Measure>, r:int<'Measure>) =
            l == int r

        static member (==) (l:int<'Measure>, Value r:LinearExpression<'Measure>) =
            int l == r

        static member (==) (Value l:LinearExpression<'Measure>, Decision.Value r:Decision<'Measure>) =
            l == r

        static member (==) (Decision.Value l:Decision<'Measure>, Value r:LinearExpression<'Measure>) =
            l == r

        static member (==) (Value l:LinearExpression<'Measure>, Value r:LinearExpression<'Measure>) =
            l == r

        static member (>==) (Value l:LinearExpression<'Measure>, r:int<'Measure>) =
            l >== int r

        static member (>==) (l:int<'Measure>, Value r:LinearExpression<'Measure>) =
            int l >== r

        static member (>==) (Value l:LinearExpression<'Measure>, Decision.Value r:Decision<'Measure>) =
            l >== r

        static member (>==) (Decision.Value l:Decision<'Measure>, Value r:LinearExpression<'Measure>) =
            l >== r

        static member (>==) (Value l:LinearExpression<'Measure>, Value r:LinearExpression<'Measure>) =
            l >== r


type DecisionType<[<Measure>] 'Measure> =
    | Boolean
    | Integer of LowerBound:int<'Measure> * UpperBound:int<'Measure>
